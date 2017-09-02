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

import operator
from weakref import WeakValueDictionary

from .reg import BaseRegister
from .special import Special, SpecialHalt, Syscall
from .mem import MemSpace
from .anchor import Anchor
from veles.data.repack import Endian


expr_cache = WeakValueDictionary()


class ConstFoldBin:
    def __init__(self, op):
        self.op = op

    def __call__(self, cls, va, vb):
        if isinstance(va, SemaConst) and isinstance(vb, SemaConst):
            return SemaConst(va.width, self.op(va.val, vb.val))


class ConstFoldBinPred:
    def __init__(self, op):
        self.op = op

    def __call__(self, cls, va, vb):
        if isinstance(va, SemaConst) and isinstance(vb, SemaConst):
            return SemaConst(1, self.op(va.val, vb.val))


def fold_const_swap(cls, va, vb):
    if isinstance(va, SemaConst) and not isinstance(vb, SemaConst):
        return cls(vb, va)


def fold_const_reass(cls, va, vb):
    if isinstance(va, cls) and isinstance(vb, SemaConst):
        return cls(va.va, cls(va.vb, vb))


def fold_const_zero(cls, va, vb):
    if isinstance(vb, SemaConst) and vb.val == 0:
        return va


class SemaExpr:
    folders = []

    def __new__(cls, *args):
        args = cls.validate_args(*args)
        all_args = cls, *args
        if all_args in expr_cache:
            return expr_cache[all_args]
        for folder in cls.folders:
            res = folder(cls, *args)
            if res is not None:
                return res
        self = super().__new__(cls)
        self.init(*args)
        expr_cache[all_args] = self
        self._args = all_args
        return self

    def __add__(self, other):
        return SemaAdd(self, other)

    __radd__ = __add__

    def __sub__(self, other):
        return SemaSub(self, other)

    def __rsub__(self, other):
        return SemaSub(other, self)

    def __mul__(self, other):
        return SemaMul(self, other)

    __rmul__ = __mul__

    def __neg__(self):
        return 0 - self

    def __and__(self, other):
        return SemaAnd(self, other)

    __rand__ = __and__

    def __or__(self, other):
        return SemaOr(self, other)

    __ror__ = __or__

    def __xor__(self, other):
        return SemaXor(self, other)

    __rxor__ = __xor__

    def __invert__(self):
        return SemaXor(self, ~0)

    def __lshift__(self, other):
        return SemaShl(self, other)

    def __rshift__(self, other):
        return SemaShr(self, other)


class SemaConst(SemaExpr):
    @classmethod
    def validate_args(cls, width, val):
        assert isinstance(width, int)
        assert width >= 0
        assert isinstance(val, int)
        return width, val

    def init(self, width, val):
        self.width = width
        self.val = val & ((1 << width) - 1)

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.width,
            self.val,
        )

    def __str__(self):
        return 'uint{}_t({:#x})'.format(self.width, self.val)


class SemaVar(SemaExpr):
    @classmethod
    def validate_args(cls, width, name):
        assert isinstance(width, int)
        assert width >= 0
        assert isinstance(name, str)
        return width, name

    def init(self, width, name):
        self.width = width
        self.name = name

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.width,
            self.name,
        )

    def __str__(self):
        return self.name


class SemaExprBinBase(SemaExpr):
    @classmethod
    def validate_args(cls, va, vb):
        if isinstance(va, int):
            assert isinstance(vb, SemaExpr)
            va = SemaConst(vb.width, va)
        assert isinstance(va, SemaExpr)
        if isinstance(vb, int):
            vb = SemaConst(va.width, vb)
        assert isinstance(vb, SemaExpr)
        assert va.width == vb.width
        return va, vb

    def init(self, va, vb):
        self.va = va
        self.vb = vb

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.va.rebuild(rebuilder),
            self.vb.rebuild(rebuilder),
        )

    def __str__(self):
        return '({} {} {})'.format(self.va, self.symbol, self.vb)


class SemaExprBin(SemaExprBinBase):
    def init(self, va, vb):
        super().init(va, vb)
        self.width = self.va.width


class SemaExprBinPred(SemaExprBinBase):
    def init(self, va, vb):
        super().init(va, vb)
        self.width = 1


class SemaAdd(SemaExprBin):
    symbol = '+'
    folders = [
        ConstFoldBin(operator.add),
        fold_const_zero,
        fold_const_swap,
        fold_const_reass,
    ]


class SemaSub(SemaExprBin):
    symbol = '-'

    def fold_rconst(cls, va, vb):
        if isinstance(vb, SemaConst):
            return va + SemaConst(vb.width, -vb.val)

    folders = [
        ConstFoldBin(operator.sub),
        fold_const_zero,
        fold_rconst,
    ]


class SemaMul(SemaExprBin):
    symbol = '*'
    folders = [
        ConstFoldBin(operator.mul),
        fold_const_swap,
        fold_const_reass,
    ]


class SemaUDiv(SemaExprBin):
    symbol = '/'


class SemaUMod(SemaExprBin):
    symbol = '%'


class SemaAddX(SemaExpr):
    @classmethod
    def validate_args(cls, va, vb, vc):
        if isinstance(va, int):
            assert isinstance(vb, SemaExpr)
            va = SemaConst(vb.width, va)
        assert isinstance(va, SemaExpr)
        if isinstance(vb, int):
            vb = SemaConst(va.width, vb)
        assert isinstance(vb, SemaExpr)
        assert va.width == vb.width
        if isinstance(vc, int):
            vc = SemaConst(1, vc)
        assert isinstance(vc, SemaExpr)
        assert vc.width == 1
        return va, vb, vc

    def init(self, va, vb, vc):
        self.va = va
        self.vb = vb
        self.vc = vc
        self.width = self.va.width

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.va.rebuild(rebuilder),
            self.vb.rebuild(rebuilder),
            self.vc.rebuild(rebuilder),
        )

    def __str__(self):
        return '$addx({}, {}, {})'.format(self.va, self.vb, self.vc)

    def fold_add(cls, va, vb, vc):
        if isinstance(vc, SemaConst) and vc.val == 0:
            return va + vb

    def fold_sub(cls, va, vb, vc):
        if isinstance(vc, SemaConst) and vc.val == 1:
            return va - ~vb

    folders = [
        fold_add,
        fold_sub,
    ]


class SemaAddFBase(SemaExpr):
    @classmethod
    def validate_args(cls, va, vb, vc):
        if isinstance(va, int):
            assert isinstance(vb, SemaExpr)
            va = SemaConst(vb.width, va)
        assert isinstance(va, SemaExpr)
        if isinstance(vb, int):
            vb = SemaConst(va.width, vb)
        assert isinstance(vb, SemaExpr)
        assert va.width == vb.width
        assert isinstance(vc, SemaExpr)
        assert vc.width == va.width
        return va, vb, vc

    def init(self, va, vb, vc):
        self.va = va
        self.vb = vb
        self.vc = vc
        self.width = 1

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.va.rebuild(rebuilder),
            self.vb.rebuild(rebuilder),
            self.vc.rebuild(rebuilder),
        )

    def fold_shorten(cls, va, vb, vc):
        if va.width != 1:
            return cls(SemaSF(va), SemaSF(vb), SemaSF(vc))

    folders = [
        fold_shorten,
    ]


class SemaCF(SemaAddFBase):
    def __str__(self):
        return '$cf({}, {}, {})'.format(self.va, self.vb, self.vc)

    def fold_const(cls, va, vb, vc):
        assert va.width == 1
        if isinstance(va, SemaConst) and isinstance(vb, SemaConst) and isinstance(vc, SemaConst):
            if va.val == 0 and vb.val == 0:
                res = 0
            elif va.val == 1 and vb.val == 1:
                res = 1
            else:
                res = vc.val ^ 1
            return SemaConst(1, res)

    folders = [
        SemaAddFBase.fold_shorten,
        fold_const,
    ]


class SemaOF(SemaAddFBase):
    def __str__(self):
        return '$of({}, {}, {})'.format(self.va, self.vb, self.vc)

    def fold_const(cls, va, vb, vc):
        assert va.width == 1
        if isinstance(va, SemaConst) and isinstance(vb, SemaConst) and isinstance(vc, SemaConst):
            return SemaConst(1, int(va.val == vb.val and va.val != vc.val))

    folders = [
        SemaAddFBase.fold_shorten,
        fold_const,
    ]


class SemaConcat(SemaExpr):
    @classmethod
    def validate_args(cls, *vals):
        assert all(isinstance(x, SemaExpr) for x in vals)
        return vals

    def init(self, *vals):
        self.vals = vals
        self.width = sum(x.width for x in vals)

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            *[
                val.rebuild(rebuilder)
                for val in self.vals
            ]
        )

    def __str__(self):
        return '$concat({})'.format(', '.join(str(val) for val in self.vals))

    def fold_one(cls, *vals):
        if len(vals) == 1:
            return vals[0]

    def fold_const(cls, *vals):
        changed = False
        res = [vals[0]]
        for val in vals[1:]:
            if isinstance(val, SemaConst) and isinstance(res[-1], SemaConst):
                shift = res[-1].width
                res[-1] = SemaConst(shift + val.width, val.val << shift | res[-1].val)
                changed = True
            else:
                res.append(val)
        if changed:
            return SemaConcat(*res)

    folders = [
        fold_const,
        fold_one,
    ]



class SemaExtr(SemaExpr):
    @classmethod
    def validate_args(cls, val, start, width):
        assert isinstance(val, SemaExpr)
        assert isinstance(start, int)
        assert isinstance(width, int)
        assert start >= 0
        assert width >= 0
        assert start + width <= val.width
        return val, start, width

    def init(self, val, start, width):
        self.val = val
        self.start = start
        self.width = width

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.val.rebuild(rebuilder),
            self.start,
            self.width,
        )

    def __str__(self):
        return '$extr({}, {}, {})'.format(self.val, self.start, self.width)

    def fold_const(cls, val, start, width):
        if isinstance(val, SemaConst):
            return SemaConst(width, val.val >> start)

    folders = [
        fold_const
    ]


class SemaSExt(SemaExpr):
    @classmethod
    def validate_args(cls, val, width):
        assert isinstance(val, SemaExpr)
        assert isinstance(width, int)
        assert width >= 0
        assert val.width <= width
        return val, width

    def init(self, val, width):
        self.val = val
        self.width = width

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.val.rebuild(rebuilder),
            self.width,
        )

    def __str__(self):
        return '$sext({}, {})'.format(self.val, self.width)


class SemaAnd(SemaExprBin):
    symbol = '&'

    def fold_same(cls, va, vb):
        if va == vb:
            return va

    folders = [
        ConstFoldBin(operator.and_),
        fold_const_swap,
        fold_const_reass,
        fold_same,
    ]


class SemaOr(SemaExprBin):
    symbol = '|'

    def fold_same(cls, va, vb):
        if va == vb:
            return va

    folders = [
        ConstFoldBin(operator.or_),
        fold_const_zero,
        fold_const_swap,
        fold_const_reass,
        fold_same,
    ]


class SemaXor(SemaExprBin):
    symbol = '^'

    def fold_same(cls, va, vb):
        if va == vb:
            return SemaConst(va.width, 0)

    folders = [
        ConstFoldBin(operator.xor),
        fold_const_zero,
        fold_const_swap,
        fold_const_reass,
        fold_same,
    ]


class SemaExprShift(SemaExprBinBase):
    @classmethod
    def validate_args(cls, va, vb):
        assert isinstance(va, SemaExpr)
        if isinstance(vb, int):
            vb = SemaConst(va.width, vb)
        assert isinstance(vb, SemaExpr)
        return va, vb

    def init(self, va, vb):
        self.va = va
        self.vb = vb
        self.width = va.width


class SemaShl(SemaExprShift):
    symbol = '<<'

    def fold_rconst(cls, va, vb):
        if isinstance(vb, SemaConst):
            if vb.val >= va.width:
                return SemaConst(va.width, 0)
            if vb.val == 0:
                return va
            return SemaConcat(
                SemaConst(vb.val, 0),
                SemaExtr(va, 0, va.width - vb.val)
            )

    folders = [
        fold_rconst,
    ]


class SemaShr(SemaExprShift):
    symbol = '>>'

    def fold_rconst(cls, va, vb):
        if isinstance(vb, SemaConst):
            if vb.val >= va.width:
                return SemaConst(va.width, 0)
            if vb.val == 0:
                return va
            return SemaZExt(
                SemaExtr(va, vb.val, va.width - vb.val),
                va.width
            )

    folders = [
        fold_rconst,
    ]


class SemaSar(SemaExprShift):
    symbol = '>>.s'

    def fold_rconst(cls, va, vb):
        if isinstance(vb, SemaConst):
            if vb.val >= va.width:
                return SemaConst(va.width, 0)
            if vb.val == 0:
                return va
            return SemaSExt(
                SemaExtr(va, vb.val, va.width - vb.val),
                va.width
            )

    folders = [
        fold_rconst,
    ]


class SemaEq(SemaExprBinPred):
    symbol = '=='

    folders = [
        ConstFoldBinPred(operator.eq),
        fold_const_swap,
    ]


class SemaSlct(SemaExpr):
    @classmethod
    def validate_args(cls, cond, va, vb):
        assert isinstance(cond, SemaExpr)
        assert cond.width == 1
        assert isinstance(va, SemaExpr)
        if isinstance(vb, int):
            vb = SemaConst(va.width, vb)
        assert isinstance(vb, SemaExpr)
        assert va.width == vb.width
        return cond, va, vb

    def init(self, cond, va, vb):
        self.cond = cond
        self.va = va
        self.vb = vb
        self.width = va.width

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.cond.rebuild(rebuilder),
            self.va.rebuild(rebuilder),
            self.vb.rebuild(rebuilder),
        )

    def fold_const(cls, cond, va, vb):
        if isinstance(cond, SemaConst):
            return va if cond.val else vb

    def fold_same(cls, cond, va, vb):
        if va == vb:
            return va

    folders = [
        fold_const,
        fold_same,
    ]

    def __str__(self):
        return '({} ? {} : {})'.format(self.cond, self.va, self.vb)


def SemaSF(val):
    assert isinstance(val, SemaExpr)
    return SemaExtr(val, val.width - 1, 1)


def SemaZExt(val, width):
    assert isinstance(val, SemaExpr)
    assert val.width < width
    if val.width == width:
        return val
    return SemaConcat(val, SemaConst(width - val.width, 0))


# Here there be ops.


class SemaOp:
    pass


class SemaSet(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, SemaVar)
        if isinstance(src, int):
            src = SemaConst(dst.width, src)
        assert isinstance(src, SemaExpr)
        assert src.width == dst.width
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst.rebuild(rebuilder),
            self.src.rebuild(rebuilder),
        )

    def str(self, indent):
        return '{} = {}'.format(self.dst, self.src)


class SemaReadAnchor(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, SemaVar)
        assert isinstance(src, Anchor)
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst.rebuild(rebuilder),
            self.src,
        )

    def str(self, indent):
        return '{} = $anchors.{}'.format(self.dst, self.src.name)


class SemaReadReg(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, SemaVar)
        assert isinstance(src, BaseRegister)
        assert src.width == dst.width
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst.rebuild(rebuilder),
            self.src,
        )

    def str(self, indent):
        return '{} = {}'.format(self.dst, self.src)


class SemaWriteReg(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, BaseRegister)
        if isinstance(src, int):
            src = SemaConst(dst.width, src)
        assert isinstance(src, SemaExpr)
        assert src.width == dst.width
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst,
            self.src.rebuild(rebuilder),
        )

    def str(self, indent):
        return '{} = {}'.format(self.dst, self.src)


class SemaSpecial(SemaOp):
    def __init__(self, special, outs, ins):
        assert isinstance(special, Special)
        assert len(outs) == len(special.outs)
        assert len(ins) == len(special.ins)
        for out, sout in zip(outs, special.outs):
            assert isinstance(out, SemaVar)
            assert out.width == sout
        for in_, sin in zip(ins, special.ins):
            assert isinstance(in_, SemaExpr)
            assert in_.width == sin
        self.special = special
        self.outs = outs
        self.ins = ins

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.special,
            [
                out.rebuild(rebuilder)
                for out in self.outs
            ],
            [
                in_.rebuild(rebuilder)
                for in_ in self.ins
            ],
        )

    def str(self, indent):
        res = '{}({})'.format(self.special.name, ', '.join([
            str(x) for x in self.ins
        ]))
        if self.outs:
            res = ', '.join([str(x) for x in self.outs]) + ' = ' + res
        return res


class SemaSpecialHalt(SemaOp):
    def __init__(self, special, ins):
        assert isinstance(special, SpecialHalt)
        assert len(ins) == len(special.ins)
        for in_, sin in zip(ins, special.ins):
            assert isinstance(in_, SemaExpr)
            assert in_.width == sin
        self.special = special
        self.ins = ins

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.special,
            [
                in_.rebuild(rebuilder)
                for in_ in self.ins
            ],
        )

    def str(self, indent):
        return '{}({})'.format(self.special.name, ', '.join([
            str(x) for x in self.ins
        ]))


class SemaSyscall(SemaOp):
    def __init__(self, syscall, ins):
        assert isinstance(syscall, Syscall)
        assert len(ins) == len(syscall.ins)
        for in_, sin in zip(ins, syscall.ins):
            assert isinstance(in_, SemaExpr)
            assert in_.width == sin
        self.syscall = syscall
        self.ins = ins

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.syscall,
            [
                in_.rebuild(rebuilder)
                for in_ in self.ins
            ],
        )

    def str(self, indent):
        return 'SYSCALL({})'.format(', '.join([self.syscall.name] + [
            str(x) for x in self.ins
        ]))


class SemaReadArg(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, SemaVar)
        assert isinstance(src, int)
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst.rebuild(rebuilder),
            self.src,
        )

    def str(self, indent):
        return '{} = $arg{}'.format(self.dst, self.src)


class SemaWriteArg(SemaOp):
    def __init__(self, dst, src):
        assert isinstance(dst, int)
        assert isinstance(src, SemaExpr)
        self.dst = dst
        self.src = src

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.dst,
            self.src.rebuild(rebuilder),
        )

    def str(self, indent):
        return '$arg{} = {}'.format(self.dst, self.src)


class SemaUnkOp(SemaOp):
    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
        )

    def str(self, indent):
        return '???'


class SemaIfElse(SemaOp):
    def __init__(self, cond, opsp, opsn):
        assert isinstance(cond, SemaExpr)
        assert isinstance(opsp, list)
        assert isinstance(opsn, list)
        assert cond.width == 1
        self.cond = cond
        self.opsp = SemaList(opsp)
        self.opsn = SemaList(opsn)

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.cond.rebuild(rebuilder),
            self.opsp.rebuild(rebuilder),
            self.opsn.rebuild(rebuilder),
        )

    def str(self, indent):
        res = 'if ({}) {{\n'.format(self.cond)
        res += self.opsp.str(indent+1)
        if self.opsn:
            res += '    ' * indent + '} else {\n'
            res += self.opsn.str(indent+1)
        res += '    ' * indent + '}'
        return res


def SemaIf(cond, opsp):
    return SemaIfElse(cond, opsp, [])


class SemaLoad(SemaOp):
    def __init__(self, val, mem, endian, addr):
        assert isinstance(val, SemaVar)
        assert isinstance(mem, MemSpace)
        assert isinstance(endian, Endian)
        assert isinstance(addr, SemaExpr)
        assert addr.width == mem.addr_width
        self.val = val
        self.mem = mem
        self.width = val.width
        self.endian = endian
        self.addr = addr

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.val.rebuild(rebuilder),
            self.mem,
            self.endian,
            self.addr.rebuild(rebuilder),
        )

    def str(self, indent):
        return '{} = {}[{}].{}.{}'.format(self.val, self.mem.name, self.addr, self.endian.name, self.width)


class SemaStore(SemaOp):
    def __init__(self, mem, endian, addr, val):
        assert isinstance(val, SemaExpr)
        assert isinstance(mem, MemSpace)
        assert isinstance(endian, Endian)
        assert isinstance(addr, SemaExpr)
        assert addr.width == mem.addr_width
        self.val = val
        self.mem = mem
        self.width = val.width
        self.endian = endian
        self.addr = addr

    def rebuild(self, rebuilder):
        return rebuilder(
            type(self),
            self.mem,
            self.endian,
            self.addr.rebuild(rebuilder),
            self.val.rebuild(rebuilder),
        )

    def str(self, indent):
        return '{}[{}].{}.{} = {}'.format(self.mem.name, self.addr, self.endian.name, self.width, self.val)


class SemaList(list):
    def rebuild(self, rebuilder):
        items = []
        for item in self:
            item = item.rebuild(rebuilder)
            if isinstance(item, list):
                items += item
            else:
                assert isinstance(item, SemaOp)
                items.append(item)
        return rebuilder(
            type(self),
            items,
        )

    def str(self, indent):
        return ''.join(
            indent * '    ' + x.str(indent) + '\n'
            for x in self
        )
