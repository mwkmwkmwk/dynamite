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

# Somebody's out to get you
# Hiding in shadows - poison arrows

from veles.data.repack import Endian
from ..core import Isa
from ..anchor import Anchor
from ..field import IsaField, IsaSubField, IsaMatch
from ..parser import ParseWord, ParseInsn, ParseSwitch, ParseAnchor
from ..insn import InsnSwitch, Insn
from ..arg import (ArgConst, ArgImm, ArgReg, ArgMem, ArgMemRRS, ArgMemRI,
                   ArgSwitch, ArgConstReg, ArgPCRel)
from ..reg import (Register, RegisterSP, RegisterPC, RegisterSpecial,
                   RegisterObservable, RegisterSplit, SubRegister)
from ..mem import MemSpace
from ..special import SpecialMode, Special, SpecialHalt, Syscall
from ..sema import (
    SemaVar, SemaConst, SemaConcat, SemaExtr, SemaSF, SemaAddX, SemaOF, SemaCF,
    SemaSar, SemaSExt, SemaZExt, SemaUDiv, SemaUMod, SemaEq,
    SemaSet, SemaReadReg, SemaWriteReg, SemaReadArg, SemaWriteArg, SemaSpecial,
    SemaIfElse, SemaLoad, SemaStore, SemaSpecialHalt, SemaSyscall, SemaIf,
    SemaReadAnchor,
)


class FalconArch:
    anchor_start = Anchor('start')
    anchor_end = Anchor('end')

    # The main register file.  16 32-bit registers.
    regs_r = [
        Register("r{}".format(x), 32)
        for x in range(16)
    ]
    # The low 16-bit halves of the registers.  Used by 16-bit operations.
    # If written, the high 16 bits are unmodified.
    regs_rh = [
        SubRegister("r{}h".format(x), reg, 0, 16)
        for x, reg in enumerate(regs_r)
    ]
    # The low 8-bit parts of the registers.  Used by 8-bit operations.
    # If written, the high 24 bits are unmodified.
    regs_rb = [
        SubRegister("r{}b".format(x), reg, 0, 8)
        for x, reg in enumerate(regs_r)
    ]

    # The 8 single-bit predicates.  Rather unwieldy in practice, so not used
    # much (for one, they lack and/or/xor/mov operations).
    regs_pred = [
        Register("p{}".format(x), 1)
        for x in range(8)
    ]
    # The 4 condition codes (carry, overflow, sign, zero).
    reg_ccc = RegisterSpecial("ccc", 1)
    reg_cco = RegisterSpecial("cco", 1)
    reg_ccs = RegisterSpecial("ccs", 1)
    reg_ccz = RegisterSpecial("ccz", 1)
    # The 3 interrupt enables.  If somebody knows what #2 is for, plese let
    # me know).
    reg_ie0 = RegisterObservable("ie0", 1)
    reg_ie1 = RegisterObservable("ie1", 1)
    reg_ie2 = RegisterObservable("ie2", 1)
    # The 3 saved interrupt enables.  Entering an interrupt stores ie* here,
    # the iret instruction restores ie* from here.
    reg_sie0 = RegisterSpecial("sie0", 1)
    reg_sie1 = RegisterSpecial("sie1", 1)
    reg_sie2 = RegisterSpecial("sie2", 1)
    # The trap active flag.  Set when entering the trap handler.  If a trap
    # is triggered while this is already set, the processor halts.
    # The flags register in all its glory.
    reg_ta = RegisterObservable("ta", 1)
    # 3 unknown bits having something to do with interrupts.
    reg_unk26 = RegisterObservable("unk26", 1)
    reg_unk27 = RegisterObservable("unk27", 1)
    reg_unk28 = RegisterObservable("unk28", 1)
    # Copies of the above, saved by interrupt entry, restored by iret.
    reg_sunk26 = RegisterSpecial("sunk26", 1)
    reg_sunk27 = RegisterSpecial("sunk27", 1)
    reg_sunk28 = RegisterSpecial("sunk28", 1)
    # 0-7: the predicates.
    regs_flag = regs_pred + [
        # 8-11: the condition codes.
        reg_ccc,
        reg_cco,
        reg_ccs,
        reg_ccz,
        # 12-15: unused.
        None, None, None, None,
        # 16-19: interrupt enables.
        reg_ie0,
        reg_ie1,
        reg_ie2,
        None,
        # 20-23: saved interrupt enables.
        reg_sie0,
        reg_sie1,
        reg_sie2,
        None,
        # 24, 25: trap active flag, unused.
        reg_ta,
        None,
        # 26-28: ???
        reg_unk26,
        reg_unk27,
        reg_unk28,
        # 29-31: ???
        reg_sunk26,
        reg_sunk27,
        reg_sunk28,
    ]
    # The assembled flags register.
    reg_flags = RegisterSplit("flags", 32, [
        (x, 1, reg)
        for x, reg in enumerate(regs_flag)
        if reg is not None
    ])
    # The crypto transfer override register (really 8-bit?).
    reg_cx = RegisterObservable("cx", 32)

    # The stack pointer.  Not truly a 32-bit register, but let's pretend so.
    # In reality, the low 2 bits are forced to 0 (it's always word-aligned),
    # and only as many bits are implemented as necessary to cover the data RAM.
    reg_sp = RegisterSP("sp", 32)
    # The program counter.  Again, fewer bits may actually be implemented.
    reg_pc = RegisterPC("pc", 32)
    # The special registers file in all its glory.
    regs_special = [
        # The interrupt vectors.  Only as many bits implemented as in the PC.
        RegisterObservable("iv0", 32),
        RegisterObservable("iv1", 32),
        None,
        # The trap vector.  Likewise with implemented bits.
        RegisterObservable("tv", 32),
        # SP & PC also accessible here.
        reg_sp,
        reg_pc,
        # The code and data transfer base registers (full 32-bit).
        RegisterObservable("xcbase", 32),
        RegisterObservable("xdbase", 32),
        # Good old flags.
        reg_flags,
        reg_cx,
        # The crypto auth mode register.  Here there be dragons.
        RegisterObservable("cauth", 32),
        # The transfer ports register.  Only has 3×3-bit FalconFields.
        RegisterObservable("xports", 32),
        # Trap status register.  Also split into FalconFields...
        RegisterSpecial("tstat", 32),
        None,
        None,
        None,
    ]

    # The data space.  32-bit addressing, 8-bit bytes.
    # Not exactly true on < v4, where only the low 12 or so bits actually
    # matter (depending on the data RAM size), and addresses might just
    # as well be treated as 16-bit, but let's ignore it for now.
    # On v4+ with the UAS option, all bits of a data address are important
    # (to match the UAS window address).
    mem_d = MemSpace("D", 8, 32)
    # The I/O register space.  32-bit addressing, 8-bit bytes.
    # Again not exactly true.  Only bits 2-17 matter on most Falcons (and
    # high 8 bits probably don't matter on any Falcon).
    mem_io = MemSpace("I", 8, 32)

    # Halts the processor, triggers an interrupt to host.  Only host can now
    # restart the processor.
    spec_halt = SpecialHalt('halt', [])
    # Waits until all xfers to/from data space have finished.
    spec_xdwait = Special('xdwait', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [])
    # Data space xfer barrier.
    spec_xdbar = Special('xdbar', SpecialMode.VOLATILE, [], [])
    # Waits until all xfers to/from code space have finished.
    spec_xcwait = Special('xcwait', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [])
    # Triggers an xfer to code space.
    spec_xcld = Special('xcld', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [32, 32])
    # Triggers an xfer to data space.
    spec_xdld = Special('xdld', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [32, 32])
    # Triggers an xfer from data space.
    spec_xdst = Special('xdst', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [32, 32])
    # Invalidates a TLB entry.
    spec_itlb = Special('itlb', SpecialMode.MEMORY | SpecialMode.VOLATILE, [], [32])
    # Queries a TLB entry by physical address.
    spec_ptlb = Special('ptlb', SpecialMode.READ_VOLATILE, [32], [32])
    # Queries TLB entries by virtual address.
    spec_vtlb = Special('vtlb', SpecialMode.READ_VOLATILE, [32], [32])
    # Sends a crypto coprocessor command.
    spec_cc = Special('cc', SpecialMode.VOLATILE, [], [5, 4, 6])
    # Sleeps until the next interrupt while parameter is true.
    spec_sleep = Special('sleep', SpecialMode.VOLATILE, [], [1])
    spec_iowr = Special('iowr', SpecialMode.VOLATILE, [], [32, 32])
    spec_iowrs = Special('iowrs', SpecialMode.VOLATILE, [], [32, 32])
    spec_iord = Special('iord', SpecialMode.VOLATILE, [32], [32])
    spec_iords = Special('iords', SpecialMode.VOLATILE, [32], [32])

    syscall_trap = Syscall('trap', [2])


class FalconFields:
    # Instruction word A - the main opcode.
    a = IsaField(8)
    # Size - 0 is byte (8-bit), 1 is halfword (16-bit), 2 is word (32-bit),
    # 3 is misc instructions.
    asz = IsaSubField(a, 6, 2)
    # First part of opcode - 0-2 select a form directly, 3 needs aopb to
    # select a form.
    aopa = IsaSubField(a, 4, 2)
    # Second part of opcode.  Selects form (if aopa is 3), or instruction
    # within a form (if aopa != 3).
    aopb = IsaSubField(a, 0, 4)

    # Instruction word B - present on all instructions on <= v3, v4+ started
    # introducing new forms without this.
    b = IsaField(8)
    # Argument 1 - a 4-bit field selecting the first register argument.
    arg1 = IsaSubField(b, 4, 4)
    # Argument 2 - a 4-bit field selecting the second register argument, or
    # an instruction within a form (for single-register forms).
    arg2 = IsaSubField(b, 0, 4)
    # Immediate op - selects an instruction within the immediate-only forms.
    # The top 2 bits of this byte are ignored in this case.
    iop = IsaSubField(b, 0, 6)
    # Predicate register field for the branch true / branch false instructions
    # (is really a part of iop, we just choose to print it nicely).
    iopp = IsaSubField(iop, 0, 3)

    # Instruction word C - present on two-register and three-register <= v3
    # forms.
    c = IsaField(8)
    # Argument 3 - a 4-bit field selecting the third register argument.
    arg3 = IsaSubField(c, 4, 4)
    # Argument 4 - selects the instruction within a form.
    arg4 = IsaSubField(c, 0, 4)

    # Instruction word immediate 8-bit.
    i8 = IsaField(8)
    # Low 5 bits of the immediate - we use this when the immediate is used
    # to select a $flags bit.
    i8f = IsaSubField(i8, 0, 5)

    # Instruction word immediate 16-bit.
    i16 = IsaField(16)
    # Low 8 bits of the immediate - for 8-bit instructions using 16-bit
    # immediate form for some strange reason.
    i16t8 = IsaSubField(i16, 0, 8)

    # Instruction word immediate 24-bit.
    i24 = IsaField(24)


class FalconSema:
    def st(width):
        addr = SemaVar(32, 'addr')
        val = SemaVar(width, 'val')
        return [
            SemaReadArg(addr, 0),
            SemaReadArg(val, 1),
            SemaStore(FalconArch.mem_d, Endian.LITTLE, addr, val)
        ]

    def ld(width):
        addr = SemaVar(32, 'addr')
        val = SemaVar(width, 'val')
        return [
            SemaReadArg(addr, 1),
            SemaLoad(val, FalconArch.mem_d, Endian.LITTLE, addr),
            SemaWriteArg(0, val),
        ]

    def clr(width):
        return [
            SemaWriteArg(0, SemaConst(width, 0)),
        ]

    def add(width, ad, as1, as2, mode):
        dst = SemaVar(width, 'dst')
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(width, 'src2')
        cin = SemaVar(1, 'cin')
        res = [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
        ]
        if mode & 1:
            res.append(SemaReadReg(cin, FalconArch.reg_ccc))
        else:
            res.append(SemaSet(cin, 0))
        is_sub = mode >> 1
        if is_sub:
            src2 = ~src2
            cin = ~cin
        res.append(SemaSet(dst, SemaAddX(src1, src2, cin)))
        if ad is not None:
            res.append(SemaWriteArg(ad, dst))
        res += [
            SemaWriteReg(FalconArch.reg_ccc, SemaCF(src1, src2, dst) ^ is_sub),
            SemaWriteReg(FalconArch.reg_cco, SemaOF(src1, src2, dst)),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]
        return res

    def cmpu(width):
        dst = SemaVar(width, 'dst')
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(width, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSet(dst, src1 - src2),
            SemaWriteReg(FalconArch.reg_ccc, ~SemaCF(src1, ~src2, dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def cmps(width):
        dst = SemaVar(width, 'dst')
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(width, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSet(dst, src1 - src2),
            SemaWriteReg(FalconArch.reg_ccc, SemaOF(src1, ~src2, dst) ^ SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def shl(width, dst, as1, as2):
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(8, 'src2')
        mask = src1.width - 1
        shcnt = src2 & mask
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaWriteArg(dst, src1 << shcnt),
            SemaIfElse(SemaEq(shcnt, 0), [
                SemaWriteReg(FalconArch.reg_ccc, 0)
            ], [
                SemaWriteReg(FalconArch.reg_ccc, SemaExtr(src1 >> (src1.width - shcnt), 0, 1))
            ])
        ]

    def shlc(width, dst, as1, as2):
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(8, 'src2')
        cin = SemaVar(1, 'cin')
        mask = src1.width - 1
        shcnt = src2 & mask
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaReadReg(cin, FalconArch.reg_ccc),
            SemaIfElse(SemaEq(shcnt, 0), [
                SemaWriteArg(dst, src1 << shcnt),
                SemaWriteReg(FalconArch.reg_ccc, 0)
            ], [
                SemaWriteArg(dst, src1 << shcnt | SemaZExt(cin, src1.width) << (shcnt - 1)),
                SemaWriteReg(FalconArch.reg_ccc, SemaExtr(src1 >> (src1.width - shcnt), 0, 1))
            ])
        ]

    def shr(width, dst, as1, as2):
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(8, 'src2')
        mask = src1.width - 1
        shcnt = src2 & mask
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaWriteArg(dst, src1 >> shcnt),
            SemaIfElse(SemaEq(shcnt, 0), [
                SemaWriteReg(FalconArch.reg_ccc, 0)
            ], [
                SemaWriteReg(FalconArch.reg_ccc, SemaExtr(src1 >> (shcnt - 1), 0, 1))
            ])
        ]

    def shrc(width, dst, as1, as2):
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(8, 'src2')
        cin = SemaVar(1, 'cin')
        mask = src1.width - 1
        shcnt = src2 & mask
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaReadReg(cin, FalconArch.reg_ccc),
            SemaIfElse(SemaEq(shcnt, 0), [
                SemaWriteArg(dst, src1 >> shcnt),
                SemaWriteReg(FalconArch.reg_ccc, 0),
            ], [
                SemaWriteArg(dst, src1 >> shcnt | SemaZExt(cin, src1.width) << (src1.width - shcnt)),
                SemaWriteReg(FalconArch.reg_ccc, SemaExtr(src1 >> (shcnt - 1), 0, 1)),
            ])
        ]

    def sar(width, dst, as1, as2):
        src1 = SemaVar(width, 'src1')
        src2 = SemaVar(8, 'src2')
        mask = src1.width - 1
        shcnt = src2 & mask
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaWriteArg(dst, SemaSar(src1, shcnt)),
            SemaIfElse(SemaEq(shcnt, 0), [
                SemaWriteReg(FalconArch.reg_ccc, 0)
            ], [
                SemaWriteReg(FalconArch.reg_ccc, SemaExtr(src1 >> (shcnt - 1), 0, 1))
            ])
        ]

    def not_(width, ad, as1):
        dst = SemaVar(width, 'dst')
        src = SemaVar(width, 'src')
        return [
            SemaReadArg(src, as1),
            SemaSet(dst, ~src),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def neg(width, ad, as1):
        dst = SemaVar(width, 'dst')
        src = SemaVar(width, 'src')
        return [
            SemaReadArg(src, as1),
            SemaSet(dst, -src),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_cco, SemaOF(0, ~src, dst)),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def mov(width, dst, src):
        tmp = SemaVar(width, 'val')
        return [
            SemaReadArg(tmp, src),
            SemaWriteArg(dst, tmp),
        ]

    def hswap(width, ad, as1):
        dst = SemaVar(width, 'dst')
        src = SemaVar(width, 'src')
        return [
            SemaReadArg(src, as1),
            SemaSet(dst, src >> (width // 2) | src << (width // 2)),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def tst(width):
        src = SemaVar(width, 'src')
        return [
            SemaReadArg(src, 0),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(src)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(src, 0)),
        ]

    # Unsized instructions start here.

    def mulu(dst, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaWriteArg(dst, SemaZExt(SemaExtr(src1, 0, 16), 32) * SemaZExt(SemaExtr(src2, 0, 16), 32))
        ]

    def muls(dst, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaWriteArg(dst, SemaSExt(SemaExtr(src1, 0, 16), 32) * SemaSExt(SemaExtr(src2, 0, 16), 32))
        ]

    def and_(ad, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaSet(dst, src1 & src2),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_ccc, 0),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def or_(ad, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaSet(dst, src1 | src2),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_ccc, 0),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def xor_(ad, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
            SemaSet(dst, src1 ^ src2),
            SemaWriteArg(ad, dst),
            SemaWriteReg(FalconArch.reg_ccc, 0),
            SemaWriteReg(FalconArch.reg_cco, 0),
            SemaWriteReg(FalconArch.reg_ccs, SemaSF(dst)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(dst, 0)),
        ]

    def t_extr(dst, src1, low, sizem1, has_sign):
        res = src1 >> low & ((SemaConst(32, 2) << sizem1) - 1)
        signbit = low + sizem1
        sign = SemaExtr(src1 >> signbit, 0, 1) & has_sign
        return [
            SemaIfElse(sign, [
                SemaWriteArg(dst, res | -(SemaConst(32, 2) << sizem1))
            ], [
                SemaWriteArg(dst, res)
            ]),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(res, 0)),
            SemaWriteReg(FalconArch.reg_ccs, sign)
        ]

    def sext(dst, as1, as2):
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        sizem1 = SemaExtr(src2, 0, 5)
        return [
            SemaReadArg(src1, as1),
            SemaReadArg(src2, as2),
        ] + FalconSema.t_extr(dst, src1, 0, sizem1, True)

    def extrs():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        low = SemaExtr(src2, 0, 5)
        sizem1 = SemaExtr(src2, 5, 5)
        return [
            SemaReadArg(src1, 1),
            SemaReadArg(src2, 2),
        ] + FalconSema.t_extr(0, src1, low, sizem1, True)

    def extr():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        low = SemaExtr(src2, 0, 5)
        sizem1 = SemaExtr(src2, 5, 5)
        return [
            SemaReadArg(src1, 1),
            SemaReadArg(src2, 2),
        ] + FalconSema.t_extr(0, src1, low, sizem1, False)

    def xbit():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        low = SemaExtr(src2, 0, 5)
        return [
            SemaReadArg(src1, 1),
            SemaReadArg(src2, 2),
        ] + FalconSema.t_extr(0, src1, low, 0, False)

    def ins():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        src3 = SemaVar(32, 'src3')
        low = SemaExtr(src3, 0, 5)
        sizem1 = SemaExtr(src3, 5, 5)
        mask = (SemaConst(32, 2) << sizem1) - 1
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaReadArg(src3, 2),
            SemaIfElse(~SemaCF(low, sizem1, low + sizem1), [
                SemaWriteArg(0, (src2 & mask) << low | (src1 & ~(mask << low)))
            ], [
                SemaWriteArg(0, src1)
            ]),
        ]

    def div():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 1),
            SemaReadArg(src2, 2),
            SemaIfElse(SemaEq(src2, 0), [
                SemaWriteArg(0, SemaConst(32, 0xffffffff)),
            ], [
                SemaWriteArg(0, SemaUDiv(src1, src2)),
            ])
        ]

    def mod():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 1),
            SemaReadArg(src2, 2),
            SemaIfElse(SemaEq(src2, 0), [
                SemaWriteArg(0, src1),
            ], [
                SemaWriteArg(0, SemaUMod(src1, src2)),
            ])
        ]

    def iord():
        addr = SemaVar(32, 'addr')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(addr, 1),
            SemaSpecial(FalconArch.spec_iord, [dst], [addr]),
            SemaWriteArg(0, dst),
        ]

    def iords():
        addr = SemaVar(32, 'addr')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(addr, 1),
            SemaSpecial(FalconArch.spec_iords, [dst], [addr]),
            SemaWriteArg(0, dst),
        ]

    def iowr():
        addr = SemaVar(32, 'addr')
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(addr, 0),
            SemaReadArg(src, 1),
            SemaSpecial(FalconArch.spec_iowr, [], [addr, src])
        ]

    def iowrs():
        addr = SemaVar(32, 'addr')
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(addr, 0),
            SemaReadArg(src, 1),
            SemaSpecial(FalconArch.spec_iowrs, [], [addr, src])
        ]

    def sethi():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(16, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaWriteArg(0, SemaConcat(SemaExtr(src1, 0, 16), src2)),
        ]

    def setb():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaWriteArg(0, src1 | SemaConst(32, 1) << SemaExtr(src2, 0, 5)),
        ]

    def clrb():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaWriteArg(0, src1 & ~(SemaConst(32, 1) << SemaExtr(src2, 0, 5))),
        ]

    def tglb():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaWriteArg(0, src1 ^ SemaConst(32, 1) << SemaExtr(src2, 0, 5)),
        ]

    def setf():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadReg(src1, FalconArch.reg_flags),
            SemaReadArg(src2, 0),
            SemaWriteReg(FalconArch.reg_flags, src1 | SemaConst(32, 1) << SemaExtr(src2, 0, 5)),
        ]

    def clrf():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadReg(src1, FalconArch.reg_flags),
            SemaReadArg(src2, 0),
            SemaWriteReg(FalconArch.reg_flags, src1 & ~(SemaConst(32, 1) << SemaExtr(src2, 0, 5))),
        ]

    def tglf():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        return [
            SemaReadReg(src1, FalconArch.reg_flags),
            SemaReadArg(src2, 0),
            SemaWriteReg(FalconArch.reg_flags, src1 ^ SemaConst(32, 1) << SemaExtr(src2, 0, 5)),
        ]

    def setf_f():
        return [
            SemaWriteArg(0, SemaConst(1, 1)),
        ]

    def clrf_f():
        return [
            SemaWriteArg(0, SemaConst(1, 0)),
        ]

    def tglf_f():
        src = SemaVar(1, 'src')
        return [
            SemaReadArg(src, 0),
            SemaWriteArg(0, ~src),
        ]

    def putf():
        src1 = SemaVar(8, 'src1')
        src2 = SemaVar(32, 'src2')
        orig = SemaVar(32, 'orig')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaReadReg(orig, FalconArch.reg_flags),
            SemaSet(dst, (src2 & 1) << src1 | (orig & ~(SemaConst(32, 1) << src1))),
            SemaWriteReg(FalconArch.reg_flags, dst),
        ]

    def putf_f():
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(src, 1),
            SemaWriteArg(0, SemaExtr(src, 0, 1)),
        ]

    def getf():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(8, 'src2')
        low = SemaExtr(src2, 0, 5)
        return [
            SemaReadReg(src1, FalconArch.reg_flags),
            SemaReadArg(src2, 1),
        ] + FalconSema.t_extr(0, src1, low, 0, False)

    def getf_f():
        src = SemaVar(1, 'src')
        return [
            SemaReadArg(src, 1),
            SemaWriteArg(0, SemaZExt(src, 32)),
            SemaWriteReg(FalconArch.reg_ccz, SemaEq(src, 0)),
            SemaWriteReg(FalconArch.reg_ccs, 0),
        ]

    def t_push(src):
        old_sp = SemaVar(32, 'old_sp')
        new_sp = SemaVar(32, 'new_sp')
        return [
            SemaReadReg(old_sp, FalconArch.reg_sp),
            SemaSet(new_sp, old_sp - 4),
            SemaWriteReg(FalconArch.reg_sp, new_sp),
            SemaStore(FalconArch.mem_d, Endian.LITTLE, new_sp, src)
        ]

    def t_pop(dst):
        old_sp = SemaVar(32, 'old_sp')
        return [
            SemaReadReg(old_sp, FalconArch.reg_sp),
            SemaLoad(dst, FalconArch.mem_d, Endian.LITTLE, old_sp),
            SemaWriteReg(FalconArch.reg_sp, old_sp + 4)
        ]

    def push():
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(src, 0),
        ] + FalconSema.t_push(src)

    def pop():
        dst = SemaVar(32, 'dst')
        return FalconSema.t_pop(dst) + [
            SemaWriteArg(0, dst),
        ]

    def addsp():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadReg(src1, FalconArch.reg_sp),
            SemaReadArg(src2, 0),
            SemaWriteReg(FalconArch.reg_sp, src1 + src2),
        ]

    def jmp():
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(src, 0),
            SemaWriteReg(FalconArch.reg_pc, src),
        ]

    def call():
        src = SemaVar(32, 'src')
        ret = SemaVar(32, 'ret')
        return [
            SemaReadAnchor(ret, FalconArch.anchor_end),
        ] + FalconSema.t_push(ret) + [
            SemaReadArg(src, 0),
            SemaWriteReg(FalconArch.reg_pc, src),
        ]

    def ret():
        dst = SemaVar(32, 'dst')
        return FalconSema.t_pop(dst) + [
            SemaWriteReg(FalconArch.reg_pc, dst),
        ]

    def iret():
        dst = SemaVar(32, 'dst')
        sie0 = SemaVar(1, 'sie0')
        sie1 = SemaVar(1, 'sie1')
        sie2 = SemaVar(1, 'sie2')
        sunk26 = SemaVar(1, 'sunk26')
        sunk27 = SemaVar(1, 'sunk27')
        sunk28 = SemaVar(1, 'sunk28')
        return FalconSema.t_pop(dst) + [
            SemaWriteReg(FalconArch.reg_pc, dst),
            SemaReadReg(sie0, FalconArch.reg_sie0),
            SemaWriteReg(FalconArch.reg_ie0, sie0),
            SemaReadReg(sie1, FalconArch.reg_sie1),
            SemaWriteReg(FalconArch.reg_ie1, sie1),
            SemaReadReg(sie2, FalconArch.reg_sie2),
            SemaWriteReg(FalconArch.reg_ie2, sie2),
            SemaReadReg(sunk26, FalconArch.reg_sunk26),
            SemaWriteReg(FalconArch.reg_unk26, sunk26),
            SemaReadReg(sunk27, FalconArch.reg_sunk27),
            SemaWriteReg(FalconArch.reg_unk27, sunk27),
            SemaReadReg(sunk28, FalconArch.reg_sunk28),
            SemaWriteReg(FalconArch.reg_unk28, sunk28),
        ]

    def halt():
        return [
            SemaSpecialHalt(FalconArch.spec_halt, [])
        ]

    def xdwait():
        return [
            SemaSpecial(FalconArch.spec_xdwait, [], [])
        ]

    def xdbar():
        return [
            SemaSpecial(FalconArch.spec_xdbar, [], [])
        ]

    def xcwait():
        return [
            SemaSpecial(FalconArch.spec_xcwait, [], [])
        ]

    def trap():
        trapnum = SemaVar(2, 'trapnum')
        return [
            SemaReadArg(trapnum, 0),
            SemaSyscall(FalconArch.syscall_trap, [trapnum]),
        ]

    def bt():
        src = SemaVar(1, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src, 0),
            SemaReadArg(dst, 1),
            SemaIf(src, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bf():
        src = SemaVar(1, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src, 0),
            SemaReadArg(dst, 1),
            SemaIf(~src, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def t_bfl(flag):
        src = SemaVar(1, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(src, flag),
            SemaReadArg(dst, 0),
            SemaIf(src, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bc():
        return FalconSema.t_bfl(FalconArch.reg_ccc)

    def bo():
        return FalconSema.t_bfl(FalconArch.reg_cco)

    def bs():
        return FalconSema.t_bfl(FalconArch.reg_ccs)

    def bz():
        return FalconSema.t_bfl(FalconArch.reg_ccz)

    def t_bnfl(flag):
        src = SemaVar(1, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(src, flag),
            SemaReadArg(dst, 0),
            SemaIf(~src, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bnc():
        return FalconSema.t_bnfl(FalconArch.reg_ccc)

    def bno():
        return FalconSema.t_bnfl(FalconArch.reg_cco)

    def bns():
        return FalconSema.t_bnfl(FalconArch.reg_ccs)

    def bnz():
        return FalconSema.t_bnfl(FalconArch.reg_ccz)

    def ba():
        cf = SemaVar(1, 'cf')
        zf = SemaVar(1, 'zf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(cf, FalconArch.reg_ccc),
            SemaReadReg(zf, FalconArch.reg_ccz),
            SemaReadArg(dst, 0),
            SemaIf(~cf & ~zf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bna():
        cf = SemaVar(1, 'cf')
        zf = SemaVar(1, 'zf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(cf, FalconArch.reg_ccc),
            SemaReadReg(zf, FalconArch.reg_ccz),
            SemaReadArg(dst, 0),
            SemaIf(cf | zf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bg():
        of = SemaVar(1, 'of')
        sf = SemaVar(1, 'sf')
        zf = SemaVar(1, 'zf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(of, FalconArch.reg_cco),
            SemaReadReg(sf, FalconArch.reg_ccs),
            SemaReadReg(zf, FalconArch.reg_ccz),
            SemaReadArg(dst, 0),
            SemaIf(~(of ^ sf) & ~zf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def ble():
        of = SemaVar(1, 'of')
        sf = SemaVar(1, 'sf')
        zf = SemaVar(1, 'zf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(of, FalconArch.reg_cco),
            SemaReadReg(sf, FalconArch.reg_ccs),
            SemaReadReg(zf, FalconArch.reg_ccz),
            SemaReadArg(dst, 0),
            SemaIf((of ^ sf) | zf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bge():
        of = SemaVar(1, 'of')
        sf = SemaVar(1, 'sf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(of, FalconArch.reg_cco),
            SemaReadReg(sf, FalconArch.reg_ccs),
            SemaReadArg(dst, 0),
            SemaIf(~of ^ sf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def bl():
        of = SemaVar(1, 'of')
        sf = SemaVar(1, 'sf')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadReg(of, FalconArch.reg_cco),
            SemaReadReg(sf, FalconArch.reg_ccs),
            SemaReadArg(dst, 0),
            SemaIf(of ^ sf, [
                SemaWriteReg(FalconArch.reg_pc, dst),
            ]),
        ]

    def xcld():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSpecial(FalconArch.spec_xcld, [], [src1, src2])
        ]

    def xdld():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSpecial(FalconArch.spec_xdld, [], [src1, src2])
        ]

    def xdst():
        src1 = SemaVar(32, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSpecial(FalconArch.spec_xdst, [], [src1, src2])
        ]

    def itlb():
        src = SemaVar(32, 'src')
        return [
            SemaReadArg(src, 0),
            SemaSpecial(FalconArch.spec_itlb, [], [src]),
        ]

    def ptlb():
        src = SemaVar(32, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src, 1),
            SemaSpecial(FalconArch.spec_ptlb, [dst], [src]),
            SemaWriteArg(0, dst),
        ]

    def vtlb():
        src = SemaVar(32, 'src')
        dst = SemaVar(32, 'dst')
        return [
            SemaReadArg(src, 1),
            SemaSpecial(FalconArch.spec_vtlb, [dst], [src]),
            SemaWriteArg(0, dst),
        ]

    def ccr():
        src1 = SemaVar(8, 'src1')
        src2 = SemaVar(32, 'src2')
        return [
            SemaReadArg(src1, 0),
            SemaReadArg(src2, 1),
            SemaSpecial(FalconArch.spec_cc, [], [SemaExtr(src1, 0, 5), SemaExtr(src2, 8, 4), SemaExtr(src2, 0, 6)]),
        ]

    def cci():
        src = SemaVar(16, 'imm')
        return [
            SemaReadArg(src, 0),
            SemaIfElse(SemaExtr(src, 15, 1), [
                SemaSpecial(FalconArch.spec_cc, [], [SemaExtr(src, 10, 5), SemaExtr(src, 0, 4), SemaExtr(src, 4, 6)]),
            ], [
                SemaWriteReg(FalconArch.reg_cx, SemaZExt(SemaExtr(src, 0, 8), 32)),
            ])
        ]

    def sleep():
        src = SemaVar(1, 'src')
        return [
            SemaReadArg(src, 0),
            SemaSpecial(FalconArch.spec_sleep, [], [src])
        ]

class FalconArgs:
    # Immediate arguments.

    # The simple 8-bit immediate (used for shift amounts, bit positions,
    # and 8-bit instructions).
    i8 = ArgImm(8, FalconFields.i8)
    # The 8-bit immediate extended to 16 bits (used for 16-bit instructions
    # and sethi).
    i8zx16 = ArgImm(16, FalconFields.i8)
    i8sx16 = ArgImm(16, FalconFields.i8, signed=True)
    # The 8-bit immediate extended to 32 bits (used for most 32-bit
    # instructions, and memory addressing).
    i8zx32 = ArgImm(32, FalconFields.i8)
    i8sx32 = ArgImm(32, FalconFields.i8, signed=True)
    # The 8-bit immediate extended to 32 bits and shifted left by 1 or 2
    # (used for memory addressing).
    i8zx32s1 = ArgImm(32, FalconFields.i8, shift=1)
    i8zx32s2 = ArgImm(32, FalconFields.i8, shift=2)
    # These select the proper 8-bit immediate based on instruction size
    # field.
    i8zxs = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, i8),
        IsaMatch(1, i8zx16),
        IsaMatch(2, i8zx32),
    ])
    i8sxs = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, i8),
        IsaMatch(1, i8sx16),
        IsaMatch(2, i8sx32),
    ])

    # The 16-bit immediate truncated to 8 bits (used by 8-bit instructions if
    # the 16-bit immediate form is selected for some weirdo reason).
    i16t8 = ArgImm(8, FalconFields.i16t8)
    # The simple 16-bit immediate (used for 16-bit instructions and sethi).
    i16 = ArgImm(16, FalconFields.i16)
    # The 16-bit immediate extended to 32 bits (used for most 32-bit
    # instructions).
    i16zx32 = ArgImm(32, FalconFields.i16)
    i16sx32 = ArgImm(32, FalconFields.i16, signed=True)
    # These select the proper 16-bit immediate based on instruction size
    # field.
    i16zxs = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, i16t8),
        IsaMatch(1, i16),
        IsaMatch(2, i16zx32),
    ])
    i16sxs = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, i16t8),
        IsaMatch(1, i16),
        IsaMatch(2, i16sx32),
    ])

    # The 24-bit immediate extended to 32 bits (used for v4 jumps/calls).
    i24zx32 = ArgImm(32, FalconFields.i24)

    # The PC-relative FalconFields (for branches).
    pc8 = ArgPCRel(FalconArch.anchor_start, 32, FalconFields.i8)
    pc16 = ArgPCRel(FalconArch.anchor_start, 32, FalconFields.i16)

    # Register arguments.

    # 32-bit registers as selected by the FalconFields.arg* FalconFields.
    r1 = ArgReg(FalconFields.arg1, FalconArch.regs_r)
    r2 = ArgReg(FalconFields.arg2, FalconArch.regs_r)
    r3 = ArgReg(FalconFields.arg3, FalconArch.regs_r)
    # 16-bit register low halves, likewise.
    r1h = ArgReg(FalconFields.arg1, FalconArch.regs_rh)
    r2h = ArgReg(FalconFields.arg2, FalconArch.regs_rh)
    r3h = ArgReg(FalconFields.arg3, FalconArch.regs_rh)
    # 8-bit register low parts, likewise.
    r1b = ArgReg(FalconFields.arg1, FalconArch.regs_rb)
    r2b = ArgReg(FalconFields.arg2, FalconArch.regs_rb)
    r3b = ArgReg(FalconFields.arg3, FalconArch.regs_rb)
    # Select 8/16/32-bit registers according to instruction size field.
    r1s = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, r1b),
        IsaMatch(1, r1h),
        IsaMatch(2, r1),
    ])
    r2s = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, r2b),
        IsaMatch(1, r2h),
        IsaMatch(2, r2),
    ])
    r3s = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, r3b),
        IsaMatch(1, r3h),
        IsaMatch(2, r3),
    ])

    # The $sp register (for use in memory addressing).
    sp = ArgConstReg(FalconArch.reg_sp)
    # The predicate registers, for use in bt/bf branch instructions.
    # This selection is really treated as part of the opcode field by
    # the encoding schame, but we pretty-print it as a register name.
    pred = ArgReg(FalconFields.iopp, FalconArch.regs_pred)
    # The flag "registers", for use in single-flag manipulation instructions.
    # This is really treated as an 8-bit immediate by the hardware, but we
    # pretty-print it as a register name.
    flag = ArgReg(FalconFields.i8f, FalconArch.regs_flag)

    # The special registers as selected by the FalconFields.arg* FalconFields.
    sr1 = ArgReg(FalconFields.arg1, FalconArch.regs_special)
    sr2 = ArgReg(FalconFields.arg2, FalconArch.regs_special)

    # Memory arguments.

    # D[$rX]
    memr8 = ArgMem(FalconArch.mem_d, 8, r1)
    memr16 = ArgMem(FalconArch.mem_d, 16, r1)
    memr32 = ArgMem(FalconArch.mem_d, 32, r1)
    memr = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, memr8),
        IsaMatch(1, memr16),
        IsaMatch(2, memr32),
    ])
    # D[$rX + imm]
    memri8 = ArgMemRI(FalconArch.mem_d, 8, r1, i8zx32)
    memri16 = ArgMemRI(FalconArch.mem_d, 16, r1, i8zx32s1)
    memri32 = ArgMemRI(FalconArch.mem_d, 32, r1, i8zx32s2)
    memri = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, memri8),
        IsaMatch(1, memri16),
        IsaMatch(2, memri32),
    ])
    # D[$sp + imm]
    memspi8 = ArgMemRI(FalconArch.mem_d, 8, sp, i8zx32)
    memspi16 = ArgMemRI(FalconArch.mem_d, 16, sp, i8zx32s1)
    memspi32 = ArgMemRI(FalconArch.mem_d, 32, sp, i8zx32s2)
    memspi = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, memspi8),
        IsaMatch(1, memspi16),
        IsaMatch(2, memspi32),
    ])
    # D[$sp + $rX * scale]
    memspr8 = ArgMemRRS(FalconArch.mem_d, 8, sp, r2, 1)
    memspr16 = ArgMemRRS(FalconArch.mem_d, 16, sp, r2, 2)
    memspr32 = ArgMemRRS(FalconArch.mem_d, 32, sp, r2, 4)
    memspr = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, memspr8),
        IsaMatch(1, memspr16),
        IsaMatch(2, memspr32),
    ])
    # D[$rX + $rY * scale]
    memrr8 = ArgMemRRS(FalconArch.mem_d, 8, r1, r2, 1)
    memrr16 = ArgMemRRS(FalconArch.mem_d, 16, r1, r2, 2)
    memrr32 = ArgMemRRS(FalconArch.mem_d, 32, r1, r2, 4)
    memrr = ArgSwitch(FalconFields.asz, [
        IsaMatch(0, memrr8),
        IsaMatch(1, memrr16),
        IsaMatch(2, memrr32),
    ])

    # I[$rX]
    ior = ArgMem(FalconArch.mem_io, 32, r1)
    # I[$rX + $rY * 4]
    iorr = ArgMemRRS(FalconArch.mem_io, 32, r1, r2, 4)
    # I[$rX + imm]
    iori = ArgMemRI(FalconArch.mem_io, 32, r1, i8zx32s2)


class FalconIsa(Isa):
    # Instruction forms.

    # The forms are named according to the kind of their arguments, in order:
    #
    # - s: sized form - means the high 2 bits of opcode select operation size
    #   (8-bit, 16-bit, or 32-bit).
    # - r: a read-only register argument
    # - w: a write-only register argument
    # - m: a read-modify-write register argument
    # - i8: an 8-bit immediate argument
    # - i16: a 16-bit immediate argument
    # - i24: a 24-bit immediate argument
    # - n: no arguments at all
    #
    # Note that Falcon encodes write-only (destination) registers in the last
    # opcode field, while we display the destination register first in
    # disassembly.  This is why the register arguments all seem out of order
    # here.  If we displayed destination last, almost all instructions would
    # have their register arguments in order...
    #
    # Also note that the forms are not always strictly followed - if a form
    # has an "m" register, some instructions in the form may actually treat
    # it as read-only, or as write-only.

    # Sized R, R, I8 - store reg to D[reg+imm].
    form_srri8 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.aopb, [
            IsaMatch(0, Insn("st", FalconSema.st(width), FalconArgs.memri, FalconArgs.r2s)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, W, I8 - three-address binary ops with immediate.
    form_srwi8 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.aopb, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 1, 2, 0), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 1, 2, 1), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 1, 2, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 1, 2, 3), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(4, Insn("shl", FalconSema.shl(width, 0, 1, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(5, Insn("shr", FalconSema.shr(width, 0, 1, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(7, Insn("sar", FalconSema.sar(width, 0, 1, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(8, Insn("ld", FalconSema.ld(width), FalconArgs.r2s, FalconArgs.memri)),
            IsaMatch(0xc, Insn("shlc", FalconSema.shlc(width, 0, 1, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(0xd, Insn("shrc", FalconSema.shrc(width, 0, 1, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i8)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, W, I16 - three-address binary ops with immediate.
    form_srwi16 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.aopb, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 1, 2, 0), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 1, 2, 1), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 1, 2, 2), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 1, 2, 3), FalconArgs.r2s, FalconArgs.r1s, FalconArgs.i16zxs)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, I8 - store reg to D[$sp+imm] and compares with immediate.
    form_sri8 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(1, Insn("st", FalconSema.st(width), FalconArgs.memspi, FalconArgs.r1s)),
            IsaMatch(4, Insn("cmpu", FalconSema.cmpu(width), FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(5, Insn("cmps", FalconSema.cmps(width), FalconArgs.r1s, FalconArgs.i8sxs)),
            IsaMatch(6, Insn("cmp", FalconSema.add(width, None, 0, 1, 2), FalconArgs.r1s, FalconArgs.i8sxs)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, I16 - compares with immediate.
    form_sri16 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(4, Insn("cmpu", FalconSema.cmpu(width), FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(5, Insn("cmps", FalconSema.cmps(width), FalconArgs.r1s, FalconArgs.i16sxs)),
            IsaMatch(6, Insn("cmp", FalconSema.add(width, None, 0, 1, 2), FalconArgs.r1s, FalconArgs.i16sxs)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized W, I8 - load from D[$sp+imm].
    form_swi8 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(0, Insn("ld", FalconSema.ld(width), FalconArgs.r1s, FalconArgs.memspi)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized RW, I8 - two-address binary ops wih immediate.
    form_smi8 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 0, 1, 0), FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 0, 1, 1), FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 0, 1, 2), FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 0, 1, 3), FalconArgs.r1s, FalconArgs.i8zxs)),
            IsaMatch(4, Insn("shl", FalconSema.shl(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(5, Insn("shr", FalconSema.shr(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(7, Insn("sar", FalconSema.sar(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(0xc, Insn("shlc", FalconSema.shlc(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.i8)),
            IsaMatch(0xd, Insn("shrc", FalconSema.shrc(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.i8)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized RW, I16 - two-address binary ops wih immediate.
    form_smi16 = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 0, 1, 0), FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 0, 1, 1), FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 0, 1, 2), FalconArgs.r1s, FalconArgs.i16zxs)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 0, 1, 3), FalconArgs.r1s, FalconArgs.i16zxs)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, R - stores and compares.
    form_srr = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg4, [
            IsaMatch(0, Insn("st", FalconSema.st(width), FalconArgs.memr, FalconArgs.r2s)),
            IsaMatch(1, Insn("st", FalconSema.st(width), FalconArgs.memspr, FalconArgs.r1s)),
            IsaMatch(4, Insn("cmpu", FalconSema.cmpu(width), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(5, Insn("cmps", FalconSema.cmps(width), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(6, Insn("cmp", FalconSema.add(width, None, 0, 1, 2), FalconArgs.r1s, FalconArgs.r2s)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized R, W - three-address unary ops.
    # Ain't compiler terminology confusing?
    form_srw = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg4, [
            IsaMatch(0, Insn("not", FalconSema.not_(width, 0, 1), FalconArgs.r2s, FalconArgs.r1s)),
            IsaMatch(1, Insn("neg", FalconSema.neg(width, 0, 1), FalconArgs.r2s, FalconArgs.r1s)),
            IsaMatch(2, Insn("mov", FalconSema.mov(width, 0, 1), FalconArgs.r2s, FalconArgs.r1s)),
            IsaMatch(3, Insn("hswap", FalconSema.hswap(width, 0, 1), FalconArgs.r2s, FalconArgs.r1s)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized W, R [!] - a funny load form.
    form_swr = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg4, [
            IsaMatch(0, Insn("ld", FalconSema.ld(width), FalconArgs.r1s, FalconArgs.memspr)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized RW, R - Two-argument binary ops.
    form_smr = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg4, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 0, 1, 0), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 0, 1, 1), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 0, 1, 2), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 0, 1, 3), FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(4, Insn("shl", FalconSema.shl(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(5, Insn("shr", FalconSema.shr(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(7, Insn("sar", FalconSema.sar(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(0xc, Insn("shlc", FalconSema.shlc(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(0xd, Insn("shrc", FalconSema.shrc(width, 0, 0, 1), FalconArgs.r1s, FalconArgs.r2b)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized RW, R - Three-argument binary ops.
    form_srrw = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg4, [
            IsaMatch(0, Insn("add", FalconSema.add(width, 0, 1, 2, 0), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(1, Insn("adc", FalconSema.add(width, 0, 1, 2, 1), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(2, Insn("sub", FalconSema.add(width, 0, 1, 2, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(3, Insn("sbb", FalconSema.add(width, 0, 1, 2, 3), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2s)),
            IsaMatch(4, Insn("shl", FalconSema.shl(width, 0, 1, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(5, Insn("shr", FalconSema.shr(width, 0, 1, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(7, Insn("sar", FalconSema.sar(width, 0, 1, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(8, Insn("ld", FalconSema.ld(width), FalconArgs.r3s, FalconArgs.memrr)),
            IsaMatch(0xc, Insn("shlc", FalconSema.shlc(width, 0, 1, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2b)),
            IsaMatch(0xd, Insn("shrc", FalconSema.shrc(width, 0, 1, 2), FalconArgs.r3s, FalconArgs.r1s, FalconArgs.r2b)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])
    # Sized RW - two-address unary ops + misc.
    form_sm = InsnSwitch(FalconFields.asz, [
        IsaMatch(val, InsnSwitch(FalconFields.arg2, [
            IsaMatch(0, Insn("not", FalconSema.not_(width, 0, 0), FalconArgs.r1s)),
            IsaMatch(1, Insn("neg", FalconSema.neg(width, 0, 0), FalconArgs.r1s)),
            IsaMatch(2, Insn("mov", FalconSema.mov(width, 0, 0), FalconArgs.r1s)),
            IsaMatch(3, Insn("hswap", FalconSema.hswap(width, 0, 0), FalconArgs.r1s)),
            IsaMatch(4, Insn("clr", FalconSema.clr(width), FalconArgs.r1s)),
            IsaMatch(5, Insn("tst", FalconSema.tst(width), FalconArgs.r1s)),
        ]))
        for val, width in [
            (0, 8),
            (1, 16),
            (2, 32),
        ]
    ])

    # I24 - v4 long jumps and calls.  Not a sized form, even though the opcode
    # is in the low range.
    form_i24 = InsnSwitch(FalconFields.asz, [
        IsaMatch(0, Insn("jmp", FalconSema.jmp(), FalconArgs.i24zx32)),
        IsaMatch(1, Insn("call", FalconSema.call(), FalconArgs.i24zx32)),
    ])

    # R, W, I8 - three-address binary ops with immediate.
    form_rwi8 = InsnSwitch(FalconFields.aopb, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8sx32)),
        IsaMatch(2, Insn("sext", FalconSema.sext(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(3, Insn("extrs", FalconSema.extrs(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(7, Insn("extr", FalconSema.extr(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(8, Insn("xbit", FalconSema.xbit(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(0xb, Insn("ins", FalconSema.ins(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(0xc, Insn("div", FalconSema.div(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(0xd, Insn("mod", FalconSema.mod(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(0xe, Insn("iords", FalconSema.iords(), FalconArgs.r2, FalconArgs.iori)),
        IsaMatch(0xf, Insn("iord", FalconSema.iord(), FalconArgs.r2, FalconArgs.iori)),
    ])
    # R, R, I8 - I/O writes to I[reg+imm]
    form_rri8 = InsnSwitch(FalconFields.aopb, [
        IsaMatch(0, Insn("iowr", FalconSema.iowr(), FalconArgs.iori, FalconArgs.r2)),
        IsaMatch(1, Insn("iowrs", FalconSema.iowrs(), FalconArgs.iori, FalconArgs.r2)),
    ])
    # R, W, I16 - three-address binary ops with immediate.
    form_rwi16 = InsnSwitch(FalconFields.aopb, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16sx32)),
        IsaMatch(3, Insn("extrs", FalconSema.extrs(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 1, 2), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(7, Insn("extr", FalconSema.extr(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(0xb, Insn("ins", FalconSema.ins(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(0xc, Insn("div", FalconSema.div(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(0xd, Insn("mod", FalconSema.mod(), FalconArgs.r2, FalconArgs.r1, FalconArgs.i16zx32)),
    ])
    # RW, I8 - two-address binary ops with immediate + mov/sethi.
    form_mi8 = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 0, 1), FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 0, 1), FalconArgs.r1, FalconArgs.i8sx32)),
        IsaMatch(2, Insn("sext", FalconSema.sext(0, 0, 1), FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(3, Insn("sethi", FalconSema.sethi(), FalconArgs.r1, FalconArgs.i8zx16)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 0, 1), FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 0, 1), FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 0, 1), FalconArgs.r1, FalconArgs.i8zx32)),
        IsaMatch(7, Insn("mov", FalconSema.mov(32, 0, 1), FalconArgs.r1, FalconArgs.i8sx32)),
        IsaMatch(9, Insn("setb", FalconSema.setb(), FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(0xa, Insn("clrb", FalconSema.clrb(), FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(0xb, Insn("tglb", FalconSema.tglb(), FalconArgs.r1, FalconArgs.i8)),
        IsaMatch(0xc, Insn("getf", FalconSema.getf_f(), FalconArgs.r1, FalconArgs.flag)),
    ])
    # RW, I16 - two-address binary ops with immediate + mov/sethi.
    form_mi16 = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 0, 1), FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 0, 1), FalconArgs.r1, FalconArgs.i16sx32)),
        IsaMatch(3, Insn("sethi", FalconSema.sethi(), FalconArgs.r1, FalconArgs.i16)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 0, 1), FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 0, 1), FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 0, 1), FalconArgs.r1, FalconArgs.i16zx32)),
        IsaMatch(7, Insn("mov", FalconSema.mov(32, 0, 1), FalconArgs.r1, FalconArgs.i16sx32)),
    ])
    # R, I8 - some misc weirdos.
    form_ri8 = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0x8, Insn("putf", FalconSema.putf_f(), FalconArgs.flag, FalconArgs.r1)),
        IsaMatch(0xc, Insn("cc", FalconSema.ccr(), FalconArgs.i8, FalconArgs.r1)),
    ])
    # I8 - branches and other immediate-only insns.
    form_i8 = InsnSwitch(FalconFields.iop, [
        IsaMatch((0x00, 0x38), Insn("bt", FalconSema.bt(), FalconArgs.pred, FalconArgs.pc8)),
        IsaMatch(0x08, Insn("bc", FalconSema.bc(), FalconArgs.pc8)),
        IsaMatch(0x09, Insn("bo", FalconSema.bo(), FalconArgs.pc8)),
        IsaMatch(0x0a, Insn("bs", FalconSema.bs(), FalconArgs.pc8)),
        IsaMatch(0x0b, Insn("bz", FalconSema.bz(), FalconArgs.pc8)),
        IsaMatch(0x0c, Insn("ba", FalconSema.ba(), FalconArgs.pc8)),
        IsaMatch(0x0d, Insn("bna", FalconSema.bna(), FalconArgs.pc8)),
        IsaMatch(0x0e, Insn("bra", FalconSema.jmp(), FalconArgs.pc8)),
        IsaMatch((0x10, 0x38), Insn("bf", FalconSema.bf(), FalconArgs.pred, FalconArgs.pc8)),
        IsaMatch(0x18, Insn("bnc", FalconSema.bnc(), FalconArgs.pc8)),
        IsaMatch(0x19, Insn("bno", FalconSema.bno(), FalconArgs.pc8)),
        IsaMatch(0x1a, Insn("bns", FalconSema.bns(), FalconArgs.pc8)),
        IsaMatch(0x1b, Insn("bnz", FalconSema.bnz(), FalconArgs.pc8)),
        IsaMatch(0x1c, Insn("bg", FalconSema.bg(), FalconArgs.pc8)),
        IsaMatch(0x1d, Insn("ble", FalconSema.ble(), FalconArgs.pc8)),
        IsaMatch(0x1e, Insn("bl", FalconSema.bl(), FalconArgs.pc8)),
        IsaMatch(0x1f, Insn("bge", FalconSema.bge(), FalconArgs.pc8)),
        IsaMatch(0x20, Insn("jmp", FalconSema.jmp(), FalconArgs.i8zx32)),
        IsaMatch(0x21, Insn("call", FalconSema.call(), FalconArgs.i8zx32)),
        IsaMatch(0x28, Insn("sleep", FalconSema.sleep(), FalconArgs.flag)),
        IsaMatch(0x30, Insn("addsp", FalconSema.addsp(), FalconArgs.i8sx32)),
        IsaMatch(0x31, Insn("setf", FalconSema.setf_f(), FalconArgs.flag)),
        IsaMatch(0x32, Insn("clrf", FalconSema.clrf_f(), FalconArgs.flag)),
        IsaMatch(0x33, Insn("tglf", FalconSema.tglf_f(), FalconArgs.flag)),
        IsaMatch(0x3c, Insn("cc", FalconSema.cci(), FalconArgs.i8zx16)),
    ])
    # I16 - branches and other immediate-only insns.
    form_i16 = InsnSwitch(FalconFields.iop, [
        IsaMatch((0x00, 0x38), Insn("bt", FalconSema.bt(), FalconArgs.pred, FalconArgs.pc16)),
        IsaMatch(0x08, Insn("bc", FalconSema.bc(), FalconArgs.pc16)),
        IsaMatch(0x09, Insn("bo", FalconSema.bo(), FalconArgs.pc16)),
        IsaMatch(0x0a, Insn("bs", FalconSema.bs(), FalconArgs.pc16)),
        IsaMatch(0x0b, Insn("bz", FalconSema.bz(), FalconArgs.pc16)),
        IsaMatch(0x0c, Insn("ba", FalconSema.ba(), FalconArgs.pc16)),
        IsaMatch(0x0d, Insn("bna", FalconSema.bna(), FalconArgs.pc16)),
        IsaMatch(0x0e, Insn("bra", FalconSema.jmp(), FalconArgs.pc16)),
        IsaMatch((0x10, 0x38), Insn("bf", FalconSema.bf(), FalconArgs.pred, FalconArgs.pc16)),
        IsaMatch(0x18, Insn("bnc", FalconSema.bnc(), FalconArgs.pc16)),
        IsaMatch(0x19, Insn("bno", FalconSema.bno(), FalconArgs.pc16)),
        IsaMatch(0x1a, Insn("bns", FalconSema.bns(), FalconArgs.pc16)),
        IsaMatch(0x1b, Insn("bnz", FalconSema.bnz(), FalconArgs.pc16)),
        IsaMatch(0x1c, Insn("bg", FalconSema.bg(), FalconArgs.pc16)),
        IsaMatch(0x1d, Insn("ble", FalconSema.ble(), FalconArgs.pc16)),
        IsaMatch(0x1e, Insn("bl", FalconSema.bl(), FalconArgs.pc16)),
        IsaMatch(0x1f, Insn("bge", FalconSema.bge(), FalconArgs.pc16)),
        IsaMatch(0x20, Insn("jmp", FalconSema.jmp(), FalconArgs.i16zx32)),
        IsaMatch(0x21, Insn("call", FalconSema.call(), FalconArgs.i16zx32)),
        IsaMatch(0x30, Insn("addsp", FalconSema.addsp(), FalconArgs.i16sx32)),
        IsaMatch(0x3c, Insn("cc", FalconSema.cci(), FalconArgs.i16)),
    ])
    # No arguments - funny control instructions.
    form_n = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0, Insn("ret", FalconSema.ret())),
        IsaMatch(1, Insn("iret", FalconSema.iret())),
        IsaMatch(2, Insn("halt", FalconSema.halt())),
        IsaMatch(3, Insn("xdwait", FalconSema.xdwait())),
        IsaMatch(6, Insn("xdbar", FalconSema.xdbar())),
        IsaMatch(7, Insn("xcwait", FalconSema.xcwait())),
        IsaMatch(8, Insn("trap", FalconSema.trap(), ArgConst(2, 0))),
        IsaMatch(9, Insn("trap", FalconSema.trap(), ArgConst(2, 1))),
        IsaMatch(0xa, Insn("trap", FalconSema.trap(), ArgConst(2, 2))),
        IsaMatch(0xb, Insn("trap", FalconSema.trap(), ArgConst(2, 3))),
    ])
    # R - more funny control instructions.
    form_r = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0, Insn("push", FalconSema.push(), FalconArgs.r1)),
        IsaMatch(1, Insn("addsp", FalconSema.addsp(), FalconArgs.r1)),
        IsaMatch(4, Insn("jmp", FalconSema.jmp(), FalconArgs.r1)),
        IsaMatch(5, Insn("call", FalconSema.call(), FalconArgs.r1)),
        IsaMatch(8, Insn("itlb", FalconSema.itlb(), FalconArgs.r1)),
        IsaMatch(9, Insn("setf", FalconSema.setf(), FalconArgs.r1b)),
        IsaMatch(0xa, Insn("clrf", FalconSema.clrf(), FalconArgs.r1b)),
        IsaMatch(0xb, Insn("tglf", FalconSema.tglf(), FalconArgs.r1b)),
    ])
    # RR - yet more funny control instructions.
    form_rr = InsnSwitch(FalconFields.arg4, [
        IsaMatch(0x0, Insn("iowr", FalconSema.iowr(), FalconArgs.ior, FalconArgs.r2)),
        IsaMatch(0x1, Insn("iowrs", FalconSema.iowrs(), FalconArgs.ior, FalconArgs.r2)),
        IsaMatch(0x4, Insn("xcld", FalconSema.xcld(), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(0x5, Insn("xdld", FalconSema.xdld(), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(0x6, Insn("xdst", FalconSema.xdst(), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(0x8, Insn("putf", FalconSema.putf(), FalconArgs.r2b, FalconArgs.r1)),
    ])
    # W - even more funny control instructions.
    form_w = InsnSwitch(FalconFields.arg2, [
        IsaMatch(0, Insn("pop", FalconSema.pop(), FalconArgs.r1)),
    ])
    # RW, R - two-address binary ops.
    form_mr = InsnSwitch(FalconFields.arg4, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 0, 1), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 0, 1), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(2, Insn("sext", FalconSema.sext(0, 0, 1), FalconArgs.r1, FalconArgs.r2b)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 0, 1), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 0, 1), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 0, 1), FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(9, Insn("setb", FalconSema.setb(), FalconArgs.r1, FalconArgs.r2b)),
        IsaMatch(0xa, Insn("clrb", FalconSema.clrb(), FalconArgs.r1, FalconArgs.r2b)),
        IsaMatch(0xb, Insn("tglb", FalconSema.tglb(), FalconArgs.r1, FalconArgs.r2b)),
    ])
    # R, W - SR movs, TLB queries, and other weird things.
    form_rw = InsnSwitch(FalconFields.arg4, [
        IsaMatch(0x0, Insn("mov", FalconSema.mov(32, 0, 1), FalconArgs.sr2, FalconArgs.r1)),
        IsaMatch(0x1, Insn("mov", FalconSema.mov(32, 0, 1), FalconArgs.r2, FalconArgs.sr1)),
        IsaMatch(0x2, Insn("ptlb", FalconSema.ptlb(), FalconArgs.r2, FalconArgs.r1)),
        IsaMatch(0x3, Insn("vtlb", FalconSema.vtlb(), FalconArgs.r2, FalconArgs.r1)),
        IsaMatch(0xc, Insn("getf", FalconSema.getf(), FalconArgs.r2, FalconArgs.r1b)),
    ])
    # R, R, W - three-address binary ops.
    form_rrw = InsnSwitch(FalconFields.arg4, [
        IsaMatch(0, Insn("mulu", FalconSema.mulu(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(1, Insn("muls", FalconSema.muls(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(2, Insn("sext", FalconSema.sext(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2b)),
        IsaMatch(3, Insn("extrs", FalconSema.extrs(), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(4, Insn("and", FalconSema.and_(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(5, Insn("or", FalconSema.or_(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(6, Insn("xor", FalconSema.xor_(0, 1, 2), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(7, Insn("extr", FalconSema.extr(), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(8, Insn("xbit", FalconSema.xbit(), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2b)),
        IsaMatch(0xc, Insn("div", FalconSema.div(), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(0xd, Insn("mod", FalconSema.mod(), FalconArgs.r3, FalconArgs.r1, FalconArgs.r2)),
        IsaMatch(0xe, Insn("iords", FalconSema.iords(), FalconArgs.r3, FalconArgs.iorr)),
        IsaMatch(0xf, Insn("iord", FalconSema.iord(), FalconArgs.r3, FalconArgs.iorr)),
    ])

    # The parser.

    # The default arguments in the following helper functions are a hack needed
    # to use the instruction words inside the function - as class-level
    # variables/FalconFields, they are otherwise inaccessible until the class is
    # constructed, and we need these functions to construct the parser.

    def parse_ab(form):
        """
        Generates the remainder of a parser for two-byte forms (a, b).
        """
        return [
            ParseWord(FalconFields.b),
            ParseInsn(form),
        ]

    def parse_abc(form):
        """
        Generates the remainder of a parser for three-byte forms (a, b, c).
        """
        return [
            ParseWord(FalconFields.b),
            ParseWord(FalconFields.c),
            ParseInsn(form),
        ]

    def parse_abi8(form):
        """
        Generates the remainder of a parser for i8 forms (a, b, i8).
        """
        return [
            ParseWord(FalconFields.b),
            ParseWord(FalconFields.i8),
            ParseInsn(form),
        ]

    def parse_abi16(form):
        """
        Generates the remainder of a parser for i16 forms (a, b, i16).
        """
        return [
            ParseWord(FalconFields.b),
            ParseWord(FalconFields.i16, 'little'),
            ParseInsn(form),
        ]

    def parse_ai24(form):
        """
        Generates the remainder of a parser for the i24 form (a, i24).
        """
        return [
            ParseWord(FalconFields.i24, 'little'),
            ParseInsn(form),
        ]

    # The parse tree for ops 0x00-0xbf.
    parser_s = ParseSwitch(FalconFields.aopa, [
        IsaMatch(0, parse_abi8(form_srri8)),
        IsaMatch(1, parse_abi8(form_srwi8)),
        IsaMatch(2, parse_abi16(form_srwi16)),
        IsaMatch(3, ParseSwitch(FalconFields.aopb, [
            IsaMatch(0, parse_abi8(form_sri8)),
            IsaMatch(1, parse_abi16(form_sri16)),
            IsaMatch(4, parse_abi8(form_swi8)),
            IsaMatch(6, parse_abi8(form_smi8)),
            IsaMatch(7, parse_abi16(form_smi16)),
            IsaMatch(8, parse_abc(form_srr)),
            IsaMatch(9, parse_abc(form_srw)),
            IsaMatch(0xa, parse_abc(form_swr)),
            IsaMatch(0xb, parse_abc(form_smr)),
            IsaMatch(0xc, parse_abc(form_srrw)),
            IsaMatch(0xd, parse_ab(form_sm)),
            IsaMatch(0xe, parse_ai24(form_i24)),
        ])),
    ])

    # The parse tree for ops 0xc0-0xff.
    parser_u = ParseSwitch(FalconFields.aopa, [
        IsaMatch(0, parse_abi8(form_rwi8)),
        IsaMatch(1, parse_abi8(form_rri8)),
        IsaMatch(2, parse_abi16(form_rwi16)),
        IsaMatch(3, ParseSwitch(FalconFields.aopb, [
            IsaMatch(0, parse_abi8(form_mi8)),
            IsaMatch(1, parse_abi16(form_mi16)),
            IsaMatch(2, parse_abi8(form_ri8)),
            IsaMatch(4, parse_abi8(form_i8)),
            IsaMatch(5, parse_abi16(form_i16)),
            IsaMatch(8, parse_ab(form_n)),
            IsaMatch(9, parse_ab(form_r)),
            IsaMatch(0xa, parse_abc(form_rr)),
            IsaMatch(0xc, parse_ab(form_w)),
            IsaMatch(0xd, parse_abc(form_mr)),
            IsaMatch(0xe, parse_abc(form_rw)),
            IsaMatch(0xf, parse_abc(form_rrw)),
        ])),
    ])

    # The parse tree root.
    parser = [
        ParseAnchor(FalconArch.anchor_start),
        ParseWord(FalconFields.a),
        ParseSwitch(FalconFields.asz, [
            IsaMatch(0, parser_s),
            IsaMatch(1, parser_s),
            IsaMatch(2, parser_s),
            IsaMatch(3, parser_u),
        ]),
        ParseAnchor(FalconArch.anchor_end),
    ]

# Somebody's out to break you
# Hiding in narrows - poison arrows
