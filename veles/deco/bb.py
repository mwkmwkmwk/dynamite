from veles.dis.sema import (
    SemaList, SemaSet, SemaReadReg, SemaWriteReg, SemaIfElse,
    SemaExtr, SemaConst, SemaVar, SemaSlct
)
from veles.dis.reg import (
    Register, RegisterSP, RegisterPC, RegisterObservable, RegisterSplit, SubRegister,
    RegisterSpecial,
)


class BasicBlock:
    def __init__(self, regstate, data, data_base, base, isa, start, end):
        self.regstate = regstate
        self.sema = SemaList()
        self.nextpc = None
        self.tmp_counter = 0
        self.insns = []
        pos = start
        while end is None or pos < end:
            res = isa.parse(data, data_base, base, pos)
            self.insns.append(res)
            if res.desync:
                break
            self.process_insn(res, 'insn_{:x}'.format(pos))
            pos += res.len
            if self.nextpc is not None:
                break
        if self.nextpc is None:
            self.fill_nextpc()

    def get_tmp(self, width):
        res = SemaVar(width, 'tmp_{}'.format(self.tmp_counter))
        self.tmp_counter += 1
        return res

    def fill_nextpc(self):
        if self.nextpc is None:
            if self.cur_insn.base is None:
                self.nextpc = self.cur_insn.end
            else:
                self.nextpc = self.cur_insn.base + self.cur_insn.end

    def read_reg(self, reg):
        if isinstance(reg, (Register, RegisterSP, RegisterObservable, RegisterSpecial)):
            if reg.name not in self.regstate:
                return [], SemaVar(reg.width, 'arg_{}'.format(reg.name))
            return [], self.regstate[reg.name]
        elif isinstance(reg, RegisterPC):
            # XXX
            assert 0
        elif isinstance(reg, RegisterSplit):
            res = []
            val = SemaConst(reg.width, 0)
            for start, sub in reg.parts:
                sres, sval = self.read_reg(sub)
                res += sres
                val |= SemaZExt(sres, reg.width) << start
            return res, val
        elif isinstance(reg, SubRegister):
            res, rv = self.read_reg(reg.parent)
            return res, SemaExtr(rv, reg.start, reg.width)
        else:
            tmp = self.get_tmp(reg.width)
            return [
                SemaReadReg(tmp, reg),
            ], tmp


    def write_reg(self, cond, reg, val):
        assert reg.width == val.width
        if isinstance(reg, (Register, RegisterSP, RegisterObservable, RegisterSpecial)):
            res, ov = self.read_reg(reg)
            if cond is not None:
                self.regstate[reg.name] = SemaSlct(cond, val, ov)
            else:
                self.regstate[reg.name] = val
            if isinstance(reg, RegisterObservable) and ov != val:
                res.append(SemaWriteReg(reg, val))
            return res
        elif isinstance(reg, RegisterPC):
            if cond is not None:
                self.fill_nextpc()
                self.nextpc = SemaSlct(cond, val, self.nextpc)
            else:
                self.nextpc = val
            return []
        elif isinstance(reg, RegisterSplit):
            res = []
            for start, sub in reg.parts:
                res += self.write_reg(cond, sub, SemaExtr(val, start, sub.width))
            return res
        elif isinstance(reg, SubRegister):
            res, ov = self.read_reg(reg.parent)
            parts = []
            if reg.start != 0:
                parts.append(SemaExtr(ov, 0, reg.start))
            parts.append(val)
            end = reg.start + reg.width
            if end != reg.parent.width:
                parts.append(SemaExtr(ov, end, reg.parent.width - end))
            nv = SemaConcat(*parts)
            self.write_reg(cond, reg.parent, nv)
        else:
            return [
                SemaWriteReg(reg, val),
            ]

    def trans_sema(self, sema, prefix, cond, vstate):
        res = SemaList()
        for op in sema:
            def rebuilder_var(cls, *args):
                if cls is SemaVar:
                    width, name = args
                    if name in vstate:
                        vwidth, val = vstate[name]
                        assert width == vwidth
                        return val
                    else:
                        return cls(width, '{}_{}'.format(prefix, name))
                return cls(*args)
            if isinstance(op, SemaSet):
                assert cond is None
                src = op.src.rebuild(rebuilder_var)
                vstate[op.dst.name] = op.dst.width, src
            elif isinstance(op, SemaReadReg):
                rs, rv = self.read_reg(op.src)
                res += rs
                vstate[op.dst.name] = op.src.width, rv
            elif isinstance(op, SemaWriteReg):
                res += self.write_reg(cond, op.dst, op.src.rebuild(rebuilder_var))
            elif isinstance(op, SemaIfElse):
                cur_cond = op.cond.rebuild(rebuilder_var)
                if isinstance(cur_cond, SemaConst):
                    if cur_cond.val:
                        self.trans_sema(op.opsp, prefix, cond, vstate),
                    else:
                        self.trans_sema(op.opsn, prefix, cond, vstate),
                else:
                    condp = cur_cond
                    condn = ~cur_cond
                    if cond is not None:
                        condp &= cond
                        condn &= cond
                    op = SemaIfElse(
                        cur_cond,
                        self.trans_sema(op.opsp, prefix, condp, vstate),
                        self.trans_sema(op.opsn, prefix, condn, vstate),
                    )
                    if op.opsp or op.opsn:
                        res.append(op)
            else:
                op = op.rebuild(rebuilder_var)
                res.append(op)
        return res

    def process_insn(self, insn, prefix):
        self.cur_insn = insn
        self.sema += self.trans_sema(insn.sema, prefix, None, {})
