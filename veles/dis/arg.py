# Copyright 2017 CodiLime
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from .st import (IsaSTImm, IsaSTReg, IsaSTMem, IsaSTAdd, IsaSTMul,
                 IsaSTUnkArg)
from .field import IsaSwitch, MatchError
from .sema import (
    SemaUnkOp, SemaSet, SemaConst, SemaReadReg, SemaWriteReg, SemaVar,
    SemaReadAnchor
)


class BaseArg:
    """
    The base class for insn argument templates.  Converts instruction fields
    to/from argument syntax tree.
    """

    def parse(self, dstate):
        """
        Parses this argument from disassembly state, generating an argument
        syntax tree.
        """
        raise NotImplementedError

    def sema_read(self, dstate, var):
        raise NotImplementedError

    def sema_write(self, dstate, expr):
        raise NotImplementedError


class ArgConst(BaseArg):
    """
    A constant-value immediate argument.  On disassembly, always emits
    the given value.  On assembly, matches immediates iff they have equal
    value.
    """

    def __init__(self, width, val):
        self.width = width
        self.val = val

    def parse(self, dstate):
        return IsaSTImm(self.width, self.val)

    def sema_read(self, dstate, var):
        return [
            SemaSet(var, SemaConst(self.width, self.val)),
        ]


class ArgImm(BaseArg):
    """
    An immediate argument, controlled by an instruction field.  The raw field
    value is processed to the immediate value as follows:

    1. If immediate width is larger than field width, the value is extended
       with 0s or 1s on the left, as determined by the "signed" parameter.
       The signed parameter can be one of:

       - False: zero-extension - extend with 0s.
       - True: sign-extension - extend with a copy of the highest field bit.

       More signed formats may be defined in the future.

    2. The value is shifted left by ``shift`` bits.

    More transformations will be defined in the future.
    """
    def __init__(self, width, field, signed=False, shift=0):
        self.width = width
        self.field = field
        self.signed = signed
        self.shift = shift
        assert signed in (False, True)

    def parse(self, dstate):
        v, m = self.field.get(dstate)
        self.field.setrm(dstate, self.field.fullmask())
        if m != self.field.fullmask():
            return IsaSTUnkArg()
        if self.signed is True:
            if v & (1 << (self.field.width - 1)):
                v |= (1 << (self.width - self.shift)) - (1 << self.field.width)
        v <<= self.shift
        return IsaSTImm(self.width, v)

    def sema_read(self, dstate, var):
        imm = self.parse(dstate)
        if isinstance(imm, IsaSTUnkArg):
            return [
                SemaUnkOp(),
            ]
        else:
            return [
                SemaSet(var, SemaConst(imm.width, imm.val)),
            ]


class ArgPCRel(BaseArg):
    """
    A PC-relative immediate argument.  Like ArgImm but the value is added to
    the address of the current instruction.  The anchor argument controls
    the exact place in the current instruction that the value is relative to
    - 'start' means the beginning of the current instruction, and additional
    anchors can be defined by the parser machine.
    """
    def __init__(self, anchor, width, field, signed=True, shift=0):
        self.anchor = anchor
        self.width = width
        self.field = field
        self.signed = signed
        self.shift = shift
        assert signed in (False, True)

    def parse(self, dstate):
        v, m = self.field.get(dstate)
        self.field.setrm(dstate, self.field.fullmask())
        if m != self.field.fullmask():
            return IsaSTUnkArg()
        if self.signed is True:
            if v & (1 << (self.field.width - 1)):
                v |= (1 << (self.width - self.shift)) - (1 << self.field.width)
        v <<= self.shift
        v += dstate.anchors[self.anchor.name]
        v &= (1 << self.width) - 1
        return IsaSTImm(self.width, v, dstate.base)

    def sema_read(self, dstate, var):
        v, m = self.field.get(dstate)
        self.field.setrm(dstate, self.field.fullmask())
        if m != self.field.fullmask():
            return IsaSTUnkArg()
        if self.signed is True:
            if v & (1 << (self.field.width - 1)):
                v |= (1 << (self.width - self.shift)) - (1 << self.field.width)
        v <<= self.shift
        anchor = SemaVar(self.width, var.name + '_anchor')
        return [
            SemaReadAnchor(anchor, self.anchor),
            SemaSet(var, anchor + v),
        ]


class ArgConstReg(BaseArg):
    """
    A fixed register argument.  On disassembly, always emits the given
    register.  On assembly only matches the given register.
    """
    def __init__(self, reg):
        self.reg = reg

    def parse(self, dstate):
        return IsaSTReg(self.reg)

    def sema_read(self, dstate, var):
        return [
            SemaReadReg(var, self.reg),
        ]

    def sema_write(self, dstate, expr):
        return [
            SemaWriteReg(self.reg, expr),
        ]

# XXX: redundant with ArgConstReg + ArgSwitch - perhaps reimplement using
# these?
class ArgReg(BaseArg):
    """
    A register argument selected from a register file by an instruction field.
    The regs argument must be a sequence of registers long enough to cover
    all possible values of the given field.
    """
    def __init__(self, field, regs):
        self.field = field
        self.regs = regs
        assert (1 << self.field.width) <= len(regs)

    def parse(self, dstate):
        v, m = self.field.get(dstate)
        self.field.setrm(dstate, self.field.fullmask())
        if m != self.field.fullmask():
            return IsaSTUnkArg()
        reg = self.regs[v]
        if reg is None:
            dstate.errors.append(MatchError('unknown register'))
            return IsaSTUnkArg()
        return IsaSTReg(reg)

    def sema_read(self, dstate, var):
        st = self.parse(dstate)
        return [
            SemaReadReg(var, st.reg),
        ]

    def sema_write(self, dstate, expr):
        st = self.parse(dstate)
        return [
            SemaWriteReg(st.reg, expr),
        ]


# XXX: ArgMem* should be generalized + merged some day.

class ArgMem(BaseArg):
    """
    A memory argument with a single-piece address.  ``arg_base`` specifies how
    to parse/emit the address.
    """
    def __init__(self, space, width, arg_base, seg=None):
        self.space = space
        self.width = width
        self.arg_base = arg_base
        self.arg_seg = seg

    def parse(self, dstate):
        base = self.arg_base.parse(dstate)
        if self.arg_seg is None:
            seg = None
        else:
            seg = self.arg_seg.parse(dstate)
        return IsaSTMem(self.space, base, seg)

    def sema_read(self, dstate, var):
        return self.arg_base.sema_read(dstate, var)


class ArgMemRI(BaseArg):
    """
    A memory argument of the form base + displacement, where base is
    a non-immediate argument (most likely register), and displacement
    is an immediate.
    """
    def __init__(self, space, width, arg_base, arg_disp, seg=None):
        self.space = space
        self.width = width
        self.arg_base = arg_base
        self.arg_disp = arg_disp
        self.arg_seg = seg

    def parse(self, dstate):
        base = self.arg_base.parse(dstate)
        idx = self.arg_disp.parse(dstate)
        if isinstance(idx, IsaSTImm) and idx.val == 0:
            expr = base
        else:
            expr = IsaSTAdd(base, idx)
        if self.arg_seg is None:
            seg = None
        else:
            seg = self.arg_seg.parse(dstate)
        return IsaSTMem(self.space, expr, seg)

    def sema_read(self, dstate, var):
        base = SemaVar(self.space.addr_width, var.name + '_base')
        idx = SemaVar(self.space.addr_width, var.name + '_idx')
        res = []
        res += self.arg_base.sema_read(dstate, base)
        res += self.arg_disp.sema_read(dstate, idx)
        res.append(SemaSet(var, base + idx))
        return res


class ArgMemRRS(BaseArg):
    """
    A memory argument of the form base + index * scale, where base and
    index are non-immediate arguments (most likely registers), and scale
    is a constant.
    """
    def __init__(self, space, width, arg_base, arg_idx, scale, seg=None):
        self.space = space
        self.width = width
        self.arg_base = arg_base
        self.arg_idx = arg_idx
        self.scale = scale
        self.arg_seg = seg

    def parse(self, dstate):
        base = self.arg_base.parse(dstate)
        idx = self.arg_idx.parse(dstate)
        if self.scale != 1:
            idx = IsaSTMul(idx, IsaSTImm(self.space.addr_width, self.scale))
        expr = IsaSTAdd(base, idx)
        if self.arg_seg is None:
            seg = None
        else:
            seg = self.arg_seg.parse(dstate)
        return IsaSTMem(self.space, expr, seg)

    def sema_read(self, dstate, var):
        base = SemaVar(self.space.addr_width, var.name + '_base')
        idx = SemaVar(self.space.addr_width, var.name + '_idx')
        res = []
        res += self.arg_base.sema_read(dstate, base)
        res += self.arg_idx.sema_read(dstate, idx)
        if self.scale != 1:
            idx = idx * self.scale
        res.append(SemaSet(var, base + idx))
        return res


class ArgSwitch(IsaSwitch):
    """
    Selects an argument based on the value of a field - see IsaSwitch
    and IsaMatch for how the matching is done.
    """
    def parse(self, dstate):
        try:
            a = self.find(dstate)
        except MatchError as e:
            dstate.errors.append(e)
            return IsaSTUnkArg()
        return a.parse(dstate)

    def sema_read(self, dstate, var):
        try:
            a = self.find(dstate)
        except MatchError as e:
            return [SemaUnkOp()]
        return a.sema_read(dstate, var)

    def sema_write(self, dstate, expr):
        try:
            a = self.find(dstate)
        except MatchError as e:
            return [SemaUnkOp()]
        return a.sema_write(dstate, expr)
