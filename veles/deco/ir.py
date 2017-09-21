from veles.data.repack import Endian


class IrVal:
    pass

class IrConst(IrVal):
    def __init__(self, width, val):
        self.width = width
        self.val = val & ((1 << width) - 1)

    def __hash__(self):
        return hash(self.val)

    def __eq__(self, other):
        if not isinstance(other, IrConst):
            return False
        return other.width == self.width and other.val == self.val

    def __str__(self):
        return 'uint{}_t(0x{:x})'.format(self.width, self.val)


class IrVar(IrVal):
    def __init__(self, block, name, width):
        self.block = block
        self.name = name
        self.width = width

    def __str__(self):
        return self.name


class IrParam(IrVar):
    def __init__(self, block, name, width, loc):
        super().__init__(block, name, width)
        self.loc = loc


class IrPhi(IrVar):
    def __init__(self, block, name, width, loc):
        super().__init__(block, name, width)
        self.loc = loc


class IrExpr(IrVar):
    def __init__(self, block, name, width):
        super().__init__(block, name, width)
        self.block.exprs.append(self)

    @classmethod
    def fold(cls, block, *args):
        pass


class IrBin(IrExpr):
    def __init__(self, block, name, width, va, vb):
        super().__init__(block, name, width)
        self.va = va
        self.vb = vb

    def ins(self):
        return [self.va, self.vb]

    def display(self):
        return '{} = {}({}, {})'.format(self.name, self.etype, self.va, self.vb)


class IrBinUni(IrBin):
    def __init__(self, block, name, va, vb):
        assert va.width == vb.width
        super().__init__(block, name, va.width, va, vb)


class IrAdd(IrBinUni):
    etype = 'ADD'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                assert va.width == vb.width
                return IrConst(va.width, va.val + vb.val)
            return block.make_expr(IrAdd, vb, va)
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return va
            if isinstance(va, IrAdd) and isinstance(va.vb, IrConst):
                r = block.make_expr(IrAdd, va.vb, vb)
                return block.make_expr(IrAdd, va.va, r)
        if isinstance(va, IrAdd) and isinstance(va.vb, IrConst):
            l = block.make_expr(IrAdd, va.va, vb)
            return block.make_expr(IrAdd, l, va.vb)


class IrSub(IrBinUni):
    etype = 'SUB'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return va
            return block.make_expr(IrAdd, va, IrConst(vb.width, -vb.val))
        if isinstance(va, IrAdd) and isinstance(va.vb, IrConst):
            l = block.make_expr(IrSub, va.va, vb)
            return block.make_expr(IrAdd, l, va.vb)
        if va == vb:
            return IrConst(va.width, 0)


class IrMul(IrBinUni):
    etype = 'MUL'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                assert va.width == vb.width
                return IrConst(va.width, va.val * vb.val)
            return block.make_expr(IrMul, vb, va)
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return vb
            if vb.val == 1:
                return va
            if isinstance(va, IrMul) and isinstance(va.vb, IrConst):
                r = block.make_expr(IrMul, va.vb, vb)
                return block.make_expr(IrMul, va.va, r)
        if isinstance(va, IrMul) and isinstance(va.vb, IrConst):
            l = block.make_expr(IrMul, va.va, vb)
            return block.make_expr(IrMul, l, va.vb)


class IrUDiv(IrBinUni):
    etype = 'UDIV'


class IrSDiv(IrBinUni):
    etype = 'SDIV'


class IrUMod(IrBinUni):
    etype = 'UMOD'


class IrSMod(IrBinUni):
    etype = 'SMOD'


class IrAnd(IrBinUni):
    etype = 'AND'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                assert va.width == vb.width
                return IrConst(va.width, va.val & vb.val)
            return block.make_expr(IrAnd, vb, va)
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return vb
            if vb == IrConst(vb.width, -1):
                return va
            if isinstance(va, IrAnd) and isinstance(va.vb, IrConst):
                r = block.make_expr(IrAnd, va.vb, vb)
                return block.make_expr(IrAnd, va.va, r)
            # XXX Concat


class IrOr(IrBinUni):
    etype = 'OR'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                assert va.width == vb.width
                return IrConst(va.width, va.val | vb.val)
            return block.make_expr(IrOr, vb, va)
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return va
            if vb == IrConst(vb.width, -1):
                return vb
            if isinstance(va, IrOr) and isinstance(va.vb, IrConst):
                r = block.make_expr(IrOr, va.vb, vb)
                return block.make_expr(IrOr, va.va, r)
            # XXX Concat


class IrXor(IrBinUni):
    etype = 'XOR'

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                assert va.width == vb.width
                return IrConst(va.width, va.val ^ vb.val)
            return block.make_expr(IrXor, vb, va)
        if isinstance(vb, IrConst):
            if vb.val == 0:
                return vb
            if isinstance(va, IrXor) and isinstance(va.vb, IrConst):
                r = block.make_expr(IrXor, va.vb, vb)
                return block.make_expr(IrXor, va.va, r)
            # XXX Concat


class IrBinLeft(IrBin):
    def __init__(self, block, name, va, vb):
        super().__init__(block, name, va.width, va, vb)


class IrShl(IrBinLeft):
    etype = 'SHL'


class IrShr(IrBinLeft):
    etype = 'SHR'


class IrSar(IrBinLeft):
    etype = 'SAR'


class IrBinPred(IrBin):
    def __init__(self, block, name, va, vb):
        assert va.width == vb.width
        super().__init__(block, name, 1, va, vb)


class IrEq(IrBinPred):
    etype = 'EQ'


class IrSlt(IrBinPred):
    etype = 'SLT'


class IrUlt(IrBinPred):
    etype = 'ULT'


class IrAddX(IrExpr):
    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width
        assert vc.width == 1
        super().__init__(block, name, va.width)
        self.va = va
        self.vb = vb
        self.vc = vc

    def ins(self):
        return [self.va, self.vb, self.vc]

    def display(self):
        return '{} = ADDX({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)

    @classmethod
    def fold(cls, block, va, vb, vc):
        if isinstance(vc, IrConst):
            if vc.val == 0:
                return block.make_expr(IrAdd, va, vb)
            else:
                nvb = block.make_expr(IrXor, vb, IrConst(vb.width, -1))
                return block.make_expr(IrSub, va, nvb)


class IrCF(IrExpr):
    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width == vc.width == 1
        super().__init__(block, name, 1)
        self.va = va
        self.vb = vb
        self.vc = vc

    def ins(self):
        return [self.va, self.vb, self.vc]

    def display(self):
        return '{} = CF({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)


class IrOF(IrExpr):
    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width == vc.width == 1
        super().__init__(block, name, 1)
        self.va = va
        self.vb = vb
        self.vc = vc

    def ins(self):
        return [self.va, self.vb, self.vc]

    def display(self):
        return '{} = OF({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)


class IrConcat(IrExpr):
    def __init__(self, block, name, *parts):
        super().__init__(block, name, sum(part.width for part in parts))
        self.parts = parts

    def ins(self):
        return self.parts

    def display(self):
        return '{} = CONCAT({})'.format(self.name, ', '.join(str(x) for x in self.parts))

    @classmethod
    def fold(cls, block, *parts):
        if len(parts) == 1:
            return parts[0]
        new_parts = [parts[0]]
        folded = False
        for part in parts[1:]:
            if isinstance(part, IrConst) and isinstance(new_parts[-1], IrConst):
                lo = new_parts[-1]
                new_parts[-1] = IrConst(lo.width + part.width, lo.val | part.val << lo.width)
                folded = True
            else:
                new_parts.append(part)
        if folded:
            return block.make_expr(IrConcat, *new_parts)


class IrExtr(IrExpr):
    def __init__(self, block, name, va, pos, width):
        super().__init__(block, name, width)
        self.va = va
        self.pos = pos

    def ins(self):
        return [self.va]

    def display(self):
        return '{} = EXTR({}, {}, {})'.format(self.name, self.va, self.pos, self.width)

    @classmethod
    def fold(cls, block, va, pos, width):
        if isinstance(va, IrConst):
            return IrConst(width, va.val >> pos)


class IrSext(IrExpr):
    def __init__(self, block, name, va, width):
        super().__init__(block, name, width)
        self.va = va

    def ins(self):
        return [self.va]

    def display(self):
        return '{} = SEXT({}, {})'.format(self.name, self.va, self.width)


class IrSlct(IrExpr):
    def __init__(self, block, name, va, vb, vc):
        assert va.width == 1
        assert vb.width == vc.width
        super().__init__(block, name, vb.width)
        self.va = va
        self.vb = vb
        self.vc = vc

    def ins(self):
        return [self.va, self.vb, self.vc]

    def display(self):
        return '{} = SLCT({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)



class IrOpRes(IrVar):
    def __init__(self, op, name, width):
        super().__init__(op.block, name, width)
        self.op = op


class IrOp:
    def __init__(self, block):
        self.block = block
        self.block.ops.append(self)


class IrLoad(IrOp):
    def __init__(self, block, name, width, mem, endian, addr):
        super().__init__(block)
        self.outs = [IrOpRes(self, name, width)]
        self.ins = [addr]
        self.mem = mem
        self.endian = endian
        self.width = width

    def __str__(self):
        return '{} = *(uint{}{}_t {} *){}'.format(
            self.outs[0],
            self.width,
            'le' if self.endian is Endian.LITTLE else 'be',
            self.mem.name,
            self.ins[0],
        )


class IrStore(IrOp):
    def __init__(self, block, mem, endian, addr, val):
        super().__init__(block)
        self.outs = []
        self.ins = [addr, val]
        self.mem = mem
        self.endian = endian

    def __str__(self):
        return '*(uint{}{}_t {} *){} = {}'.format(
            self.ins[1].width,
            'le' if self.endian is Endian.LITTLE else 'be',
            self.mem.name,
            self.ins[0],
            self.ins[1],
        )


class IrReadReg(IrOp):
    def __init__(self, block, name, width, reg):
        super().__init__(block)
        self.outs = [IrOpRes(self, name, width)]
        self.ins = []
        self.reg = reg

    def __str__(self):
        return '{} = ${}'.format(
            self.outs[0],
            self.reg.name,
        )


class IrWriteReg(IrOp):
    def __init__(self, block, reg, val):
        super().__init__(block)
        self.outs = []
        self.reg = reg
        self.ins = [val]

    def __str__(self):
        return '${} = {}'.format(
            self.reg.name,
            self.ins[0],
        )


class IrSpecial(IrOp):
    def __init__(self, block, name, special, ins):
        super().__init__(block)
        self.special = special
        self.ins = list(ins)
        self.outs = [
            IrOpRes(
                self,
                name if len(special.outs) < 2 else '{}_{}'.format(name, idx),
                width,
            )
            for idx, width in enumerate(special.outs)
        ]

    def __str__(self):
        res = '{}({})'.format(self.special.name, ', '.join(str(x) for x in self.ins))
        if self.outs:
            res = '{} = {}'.format(', '.join(str(x) for x in self.outs), res)
        return res


class IrFinish:
    def __init__(self, block):
        self.block = block


class IrHalt(IrFinish):
    def __init__(self, block, special, ins):
        super().__init__(block)
        self.special = special
        self.ins = list(ins)


class IrGoto(IrFinish):
    def __init__(self, block, dst, extra):
        super().__init__(block)
        self.dst = dst
        self.extra = extra


class IrJump(IrFinish):
    def __init__(self, block, addr, extra):
        super().__init__(block)
        self.addr = addr
        self.extra = extra


class IrCond(IrFinish):
    def __init__(self, block, cond, finp, finn):
        super().__init__(block)
        self.cond = cond
        self.finp = finp
        self.finn = finn


class IrCall(IrFinish):
    def __init__(self, block, tree, extra):
        super().__init__(block)
        self.tree = tree
        self.extra = extra
        # XXX returns


# XXX IrReturn
