from .forest import DecoBlock
from .ir import (
    IrConst,
    IrParam, IrPhi,
    IrAdd, IrSub, IrMul, IrUDiv, IrUMod,
    IrAnd, IrOr, IrXor,
    IrAddX, IrCF, IrOF,
    IrShl, IrShr, IrSar,
    IrEq,
    IrConcat, IrExtr, IrSext,
    IrSlct,
    IrReadReg, IrWriteReg, IrLoad, IrStore, IrSpecial, IrHalt,
    IrGoto, IrJump, IrCond, IrCall
)

from .bb import BasicBlock
from ..dis.reg import (
    Register, RegisterSP, RegisterPC, RegisterObservable, RegisterSplit, SubRegister,
    RegisterSpecial,
)
from ..dis.sema import (
    SemaConst, SemaSlct, SemaVar,
    SemaAdd, SemaSub, SemaMul, SemaUDiv, SemaUMod,
    SemaAnd, SemaOr, SemaXor,
    SemaAddX, SemaCF, SemaOF,
    SemaShl, SemaShr, SemaSar,
    SemaEq,
    SemaExtr, SemaConcat, SemaSExt,
    SemaSlct,
    SemaSet, SemaReadReg, SemaWriteReg, SemaLoad, SemaStore, SemaSpecial, SemaSpecialHalt, SemaIfElse,
)


class Translator:
    def __init__(self, block):
        self.block = block
        self.segment = block.segment

    def xlat_block(self):
        self.regstate = dict(self.block.regstate_in)
        self.pos = self.pos = self.block.pos
        self.block.raw_insns = []
        self.block.insn_starts = set()
        self.halted = False
        self.nextpc = None
        while True:
            self.block.insn_starts.add(self.pos)
            insn = self.segment.parse_insn(self.pos)
            self.block.raw_insns.append(insn)
            self.end = self.pos + insn.len
            self.xlat_insn(insn)
            self.pos = self.end
            if self.halted:
                return
            if self.pos in self.segment.blocks:
                self.fill_nextpc()
                return

    def fill_nextpc(self):
        if self.nextpc is None:
            end = IrConst(self.segment.isa.pc_width, self.end)
            if self.segment.base is None:
                self.nextpc = end
            else:
                raise NotImplementedError
            self.halted = True

    def read_reg(self, reg):
        if isinstance(reg, (Register, RegisterSP, RegisterObservable, RegisterSpecial)):
            if reg not in self.regstate:
                return self.block.tree.root.make_arg(reg)
            return self.regstate[reg]
        elif isinstance(reg, RegisterPC):
            raise NotImplementedError
        elif isinstance(reg, RegisterSplit):
            parts = []
            bits = 0
            for start, sub in reg.parts:
                if start != bits:
                    assert start > bits
                    parts.append(IrConst(start - bits, 0))
                    bits = start
                sval = self.read_reg(sub)
                parts.append(sval)
                bits += sval.width
            if bits != reg.width:
                assert reg.width > bits
                parts.append(IrConst(reg.width - bits, 0))
            return self.block.make_expr(IrConcat, *parts)
        elif isinstance(reg, SubRegister):
            rv = self.read_reg(reg.parent)
            return self.block.make_expr(IrExtr, rv, reg.start, reg.width)
        else:
            op = IrReadReg(self.block, 'reg_{:x}_{}'.format(self.pos, reg.name), reg.width, reg)
            return op.outs[0]

    def write_reg(self, cond, reg, val):
        assert reg.width == val.width
        if isinstance(reg, (Register, RegisterSP, RegisterObservable, RegisterSpecial)):
            ov = self.read_reg(reg)
            if cond is not None:
                val = self.block.make_expr(IrSlct, cond, val, ov)
            self.regstate[reg] = val
            if isinstance(reg, RegisterObservable) and ov != val:
                IrWriteReg(self.block, reg, val)
        elif isinstance(reg, RegisterPC):
            if cond is not None:
                self.fill_nextpc()
                self.nextpc = self.block.make_expr(IrSlct, cond, val, self.nextpc)
            else:
                self.nextpc = val
            self.halted = True
        elif isinstance(reg, RegisterSplit):
            for start, sub in reg.parts:
                self.write_reg(cond, sub, self.block.make_expr(IrExtr, val, start, sub.width))
        elif isinstance(reg, SubRegister):
            ov = self.read_reg(reg.parent)
            parts = []
            if reg.start != 0:
                parts.append(self.block.make_expr(IrExtr, ov, 0, reg.start))
            parts.append(val)
            end = reg.start + reg.width
            if end != reg.parent.width:
                parts.append(self.block.make_expr(IrExtr, ov, end, reg.parent.width - end))
            nv = self.block.make_expr(IrConcat, *parts)
            self.write_reg(cond, reg.parent, nv)
        else:
            IrWriteReg(self.block, reg, val)

    def xlat_insn(self, insn):
        self.xlat_sema_ops(insn.sema, None, {})

    def load(self, width, mem, endian, addr):
        op = IrLoad(self.block, 'load_{:x}'.format(self.pos), width, mem, endian, addr)
        return op.outs[0]

    def store(self, mem, endian, addr, val):
        IrStore(self.block, mem, endian, addr, val)

    def xlat_sema_expr(self, expr, vstate):
        bin_xlat = {
            SemaAdd: IrAdd,
            SemaSub: IrSub,
            SemaMul: IrMul,
            SemaUDiv: IrUDiv,
            SemaUMod: IrUMod,
            SemaAnd: IrAnd,
            SemaOr: IrOr,
            SemaXor: IrXor,
            SemaShl: IrShl,
            SemaShr: IrShr,
            SemaSar: IrSar,
            SemaEq: IrEq,
        }
        tri_xlat = {
            SemaAddX: IrAddX,
            SemaCF: IrCF,
            SemaOF: IrOF,
        }
        if isinstance(expr, SemaVar):
            return vstate[expr.name]
        elif isinstance(expr, SemaConst):
            return IrConst(expr.width, expr.val)
        elif type(expr) in bin_xlat:
            return self.block.make_expr(
                bin_xlat[type(expr)],
                self.xlat_sema_expr(expr.va, vstate),
                self.xlat_sema_expr(expr.vb, vstate),
            )
        elif type(expr) in tri_xlat:
            return self.block.make_expr(
                tri_xlat[type(expr)],
                self.xlat_sema_expr(expr.va, vstate),
                self.xlat_sema_expr(expr.vb, vstate),
                self.xlat_sema_expr(expr.vc, vstate),
            )
        elif isinstance(expr, SemaExtr):
            return self.block.make_expr(
                IrExtr,
                self.xlat_sema_expr(expr.val, vstate),
                expr.start,
                expr.width,
            )
        elif isinstance(expr, SemaSExt):
            return self.block.make_expr(
                IrSext,
                self.xlat_sema_expr(expr.val, vstate),
                expr.width,
            )
        elif isinstance(expr, SemaConcat):
            return self.block.make_expr(
                IrConcat,
                *[
                    self.xlat_sema_expr(part, vstate)
                    for part in expr.vals
                ]
            )
        else:
            print(expr)
            raise NotImplementedError

    def xlat_sema_op(self, op, cond, vstate):
        if isinstance(op, SemaSet):
            if cond is not None:
                raise NotImplementedError
            vstate[op.dst.name] = self.xlat_sema_expr(op.src, vstate)
        elif isinstance(op, SemaReadReg):
            vstate[op.dst.name] = self.read_reg(op.src)
        elif isinstance(op, SemaWriteReg):
            self.write_reg(cond, op.dst, self.xlat_sema_expr(op.src, vstate))
        elif isinstance(op, SemaLoad):
            if cond is not None:
                raise NotImplementedError
            addr = self.xlat_sema_expr(op.addr, vstate)
            vstate[op.val.name] = self.load(op.width, op.mem, op.endian, addr)
        elif isinstance(op, SemaStore):
            if cond is not None:
                raise NotImplementedError
            addr = self.xlat_sema_expr(op.addr, vstate)
            val = self.xlat_sema_expr(op.val, vstate)
            self.store(op.mem, op.endian, addr, val)
        elif isinstance(op, SemaSpecial):
            if cond is not None:
                raise NotImplementedError
            iop = IrSpecial(self.block, 'special_{:x}'.format(self.pos), op.special, [
                self.xlat_sema_expr(val, vstate)
                for val in op.ins
            ])
            for var, val in zip(op.outs, iop.outs):
                vstate[var.name] = val
        elif isinstance(op, SemaSpecialHalt):
            if cond is not None:
                raise NotImplementedError
            # XXX
            raise NotImplementedError
            self.halted = True
        elif isinstance(op, SemaIfElse):
            cur_cond = self.xlat_sema_expr(op.cond, vstate)
            if isinstance(cur_cond, IrConst):
                if cur_cond.val:
                    self.xlat_sema_ops(op.opsp, cond, vstate),
                else:
                    self.xlat_sema_ops(op.opsn, cond, vstate),
            else:
                condp = cur_cond
                condn = self.block.make_expr(IrXor, cur_cond, IrConst(1, 1))
                if cond is not None:
                    condp = self.block.make_expr(IrAnd, condp, cond)
                    condn = self.block.make_expr(IrAnd, condn, cond)
                self.xlat_sema_ops(op.opsp, condp, vstate),
                self.xlat_sema_ops(op.opsn, condn, vstate),
        else:
            raise NotImplementedError

    def xlat_sema_ops(self, sema, cond, vstate):
        for op in sema:
            self.xlat_sema_op(op, cond, vstate)


class MachineBaseBlock(DecoBlock):
    def __init__(self, segment):
        super().__init__()
        self.segment = segment
        self.phis = {}

    def sub_init_input(self, finish):
        self.regstate_in = dict(finish.extra)
        self.regstate_in.update(self.phis)

    def add_input(self, finish):
        all_regs = set(finish.extra) | set(self.regstate_in)
        for reg in all_regs:
            if reg not in finish.extra or reg not in self.regstate_in or finish.extra[reg] != self.regstate_in[reg]:
                if reg not in self.phis:
                    phi = IrPhi(self, 'phi_{:x}_{}'.format(self.pos, reg.name), reg.width, reg)
                    self.phis[reg] = phi
                    self.regstate_in[reg] = phi
                    self.invalidate()


class MachineBlock(MachineBaseBlock):
    def __init__(self, segment, pos):
        super().__init__(segment)
        self.pos = pos
        self.segment.add_block(self)
        self.arg_cache = {}

    def sub_init_entry(self):
        self.regstate_in = {}

    def sub_process(self):
        xlat = Translator(self)
        xlat.xlat_block()
        self.segment.add_insns(self)
        if xlat.nextpc is None:
            self.finish = None
        else:
            dst = self.tree.forest.mark_block(MachineEndBlock, self.segment, xlat.end, xlat.nextpc)
            self.finish = IrGoto(self, dst, xlat.regstate)

    def sub_invalidate(self):
        self.segment.del_insns(self)

    def get_default_name(self):
        return 'block_{:x}'.format(self.pos)

    def get_func_name(self):
        return 'func_{:x}'.format(self.pos)

    def make_arg(self, reg):
        assert self.parent is None
        if reg in self.arg_cache:
            return self.arg_cache[reg]
        res = IrParam(self, 'arg_{}'.format(reg.name), reg.width, reg)
        self.arg_cache[reg] = res
        return res


class MachineEndBlock(MachineBaseBlock):
    def __init__(self, segment, pos, target):
        super().__init__(segment)
        self.pos = pos
        self.target = target

    def sub_process(self):
        if isinstance(self.target, IrConst):
            block = self.tree.forest.mark_block(MachineBlock, self.segment, self.target.val)
            if block.tree == self.tree or block.tree is None:
                self.finish = IrGoto(self, block, self.regstate_in)
            else:
                self.finish = IrCall(self, block.tree, self.regstate_in)
        elif isinstance(self.target, IrSlct):
            tgtp = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.vb)
            tgtn = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.vc)
            self.finish = IrCond(self, self.target.va,
                IrGoto(self, tgtp, self.regstate_in),
                IrGoto(self, tgtn, self.regstate_in),
            )
        else:
            self.finish = IrJump(self, self.target, self.regstate_in)


class MachineSegment:
    def __init__(self, isa, data, data_base, base):
        self.isa = isa
        self.data = data
        self.data_base = data_base
        self.base = base
        self.blocks = {}
        self.insns = {}

    def add_block(self, block):
        self.blocks[block.pos] = block
        if block.pos in self.insns:
            for other in set(self.insns[block.pos]):
                other.invalidate()

    def add_insns(self, block):
        for pos in block.insn_starts:
            if pos not in self.insns:
                self.insns[pos] = set()
            self.insns[pos].add(block)

    def del_insns(self, block):
        for pos in block.insn_starts:
            if pos in self.insns:
                self.insns[pos].remove(block)
                if not self.insns[pos]:
                    del self.insns[pos]

    def parse_insn(self, pos):
        return self.isa.parse(self.data, self.data_base, self.base, pos)
