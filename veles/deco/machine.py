from .forest import DecoBlock, DecoReturn
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
    IrOpRes,
    IrReadReg, IrWriteReg, IrLoad, IrStore, IrSpecial, IrHalt,
    IrGoto, IrJump, IrCond, IrCall, IrReturn,
)

from .bb import BasicBlock
from ..dis.reg import (
    Register, RegisterSP, RegisterPC, RegisterObservable, RegisterSplit, SubRegister,
    RegisterSpecial, BaseRegister,
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
from veles.data.repack import Endian


class StackSlot:
    def __init__(self, mem, base, offset, width, endian):
        self.mem = mem
        self.base = base
        self.offset = offset
        self.width = width
        self.endian = endian
        self.name = 'stack_{}_{:x}_{}{}{}'.format(
            self.base.name,
            self.offset,
            self.mem.name,
            self.width,
            'le' if self.endian is Endian.LITTLE else 'be'
        )

    def __str__(self):
        return '{}[{} + {:x}].{}{}'.format(
            self.mem.name,
            self.base,
            self.offset,
            'le' if self.endian is Endian.LITTLE else 'be',
            self.width,
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
            slot = self.block.tree.root.get_stack_slot(op.mem, addr, op.width, op.endian)
            if slot is None:
                vstate[op.val.name] = self.load(op.width, op.mem, op.endian, addr)
            else:
                if slot in self.regstate:
                    vstate[op.val.name] = self.regstate[slot]
                else:
                    vstate[op.val.name] = self.block.tree.root.make_arg(slot)
        elif isinstance(op, SemaStore):
            if cond is not None:
                raise NotImplementedError
            addr = self.xlat_sema_expr(op.addr, vstate)
            val = self.xlat_sema_expr(op.val, vstate)
            slot = self.block.tree.root.get_stack_slot(op.mem, addr, val.width, op.endian)
            if slot is None:
                self.store(op.mem, op.endian, addr, val)
            else:
                self.regstate[slot] = val
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
            self.block.finish = IrHalt(self.block, op.special, [
                self.xlat_sema_expr(val, vstate)
                for val in op.ins
            ])
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


class MachineReturn(DecoReturn):
    def __init__(self, tree, addr, reg_clobber, stack_offset):
        super().__init__(tree)
        self.addr = addr
        self.reg_clobber = reg_clobber
        self.stack_offset = stack_offset
        self.name = 'retpath_{}'.format(addr.name)

    def update(reg_clobber, stack_offset):
        changed = False
        stack_regs = set(stack_offset) | set(self.stack_offset)
        for loc in stack_regs:
            if loc not in self.stack_offset:
                self.stack_offset[loc] = 0
                changed = True
            if loc in stack_offset:
                new_off = stack_offset[loc]
            else:
                new_off = 0
            if new_off != self.stack_offset[loc] and self.stack_offset[loc] is not None:
                self.stack_offset[loc] = None
                changed = True
        for reg in reg_clobber:
            if reg not in self.reg_clobber:
                changed = True
                self.reg_clobber.add(reg)
        if changed:
            self.invalidate()

    def invalidate(self):
        pass


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

    def sub_init_entry(self):
        self.regstate_in = {}
        self.arg_cache = {}
        self.forbidden_stack_slots = set()
        self.stack_slots = {}
        self.ret_paths = []
        self.ret_path_cache = {}

    def sub_process(self):
        xlat = Translator(self)
        xlat.xlat_block()
        self.segment.add_insns(self)
        if xlat.nextpc is not None:
            dst = self.tree.forest.mark_block(MachineEndBlock, self.segment, xlat.end, xlat.nextpc)
            self.finish = IrGoto(self, dst, xlat.regstate)

    def sub_invalidate(self):
        self.segment.del_insns(self)

    def get_default_name(self):
        return 'block_{:x}'.format(self.pos)

    def get_func_name(self):
        return 'func_{:x}'.format(self.pos)

    def get_stack_slot(self, mem, addr, width, endian):
        assert self.parent is None
        offset = 0
        if isinstance(addr, IrAdd) and isinstance(addr.vb, IrConst):
            offset = addr.vb.val
            addr = addr.va
        if not isinstance(addr, IrParam):
            return
        if not isinstance(addr.loc, RegisterSP):
            return
        args = (mem, addr.loc, offset, width, endian)
        if args in self.stack_slots:
            return self.stack_slots[args]
        if offset in self.forbidden_stack_slots:
            return
        res = StackSlot(mem, addr.loc, offset, width, endian)
        self.stack_slots[args] = res
        return res

    def make_arg(self, loc):
        assert self.parent is None
        if loc in self.arg_cache:
            return self.arg_cache[loc]
        name = 'arg_{}'.format(loc.name)
        res = IrParam(self, name, loc.width, loc)
        self.arg_cache[loc] = res
        return res

    def mark_ret_path(self, addr, regstate):
        clobber = {
            loc
            for loc in regstate
            if isinstance(loc, BaseRegister)
        }
        stack_offset = {}
        for loc, val in regstate.items():
            if not isinstance(loc, RegisterSP):
                continue
            base = val
            offset = 0
            if isinstance(base, IrAdd) and isinstance(base.vb, IrConst):
                offset = base.vb.val
                base = base.va
            if not isinstance(base, IrParam) or base.loc != loc:
                stack_offset[loc] = None
            else:
                stack_offset[loc] = offset
        if addr in self.ret_path_cache:
            path = self.ret_path_cache[addr]
            path.update(clobber, stack_offset)
            return path
        else:
            path = MachineReturn(self.tree, addr, clobber, stack_offset)
            self.ret_path_cache[addr] = path
            self.ret_paths.append(path)
            return path


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
                # XXX returns
                self.finish = IrCall(self, block.tree, self.regstate_in, {})
        elif isinstance(self.target, IrSlct):
            tgtp = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.vb)
            tgtn = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.vc)
            self.finish = IrCond(self, self.target.va,
                IrGoto(self, tgtp, self.regstate_in),
                IrGoto(self, tgtn, self.regstate_in),
            )
        elif isinstance(self.target, IrParam):
            self.finish = IrReturn(
                self,
                self.tree.root.mark_ret_path(self.target.loc, self.regstate_in),
                self.regstate_in,
            )
        else:
            def peel_stack_phi(var):
                if not isinstance(var, IrOpRes):
                    return
                if not isinstance(var.op, IrLoad):
                    return
                base = var.op.ins[0]
                if isinstance(base, IrAdd) and isinstance(base.vb, IrConst):
                    base = base.va
                if not isinstance(base, IrPhi):
                    return
                if not isinstance(base.loc, RegisterSP):
                    return
                return base.block
            new_root = peel_stack_phi(self.target)
            if new_root is not None:
                self.tree.forest.mark_function(new_root)
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
