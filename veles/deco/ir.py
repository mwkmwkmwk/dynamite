from veles.data.repack import Endian


def const_cuts(width, val):
    res = set()
    pos = 0
    pv = val & 1
    while pos < width:
        cv = val >> pos & 1
        if cv != pv:
            res.add(pos)
        pos += 1
        pv = cv
    return res


def cut_ranges(width, *args):
    cuts = set()
    for arg in args:
        cuts |= arg
    scuts = sorted(cuts)
    return zip([0] + scuts, scuts + [width])


def lo_mask(mask):
    return (1 << mask.bit_length()) - 1


class IrVal:
    def cuts(self):
        return set()


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

    def const_cuts(self):
        return const_cuts(self.width, self.val)


class IrVar(IrVal):
    def __init__(self, block, name, width):
        self.block = block
        self.name = name
        self.width = width
        assert self.width > 0

    def __str__(self):
        return self.name


class IrParam(IrVar):
    def __init__(self, block, name, width, loc):
        super().__init__(block, name, width)
        self.loc = loc


class IrCallRes(IrVar):
    def __init__(self, block, name, width, ret_path, res):
        super().__init__(block, name, width)
        self.ret_path = ret_path
        self.res = res


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

    def live_ins(self, mask):
        return [
            (self.va, -1),
            (self.vb, -1),
        ]

    def display(self):
        return '{} = {}({}, {})'.format(self.name, self.etype, self.va, self.vb)

    commutative = False
    associative = False

    @classmethod
    def fold_const(cls, block, width, a, b):
        pass

    @classmethod
    def fold_const_right(cls, block, va, b):
        pass

    @classmethod
    def fold_same(cls, block, va):
        pass

    @classmethod
    def fold_other(cls, block, va, vb):
        pass

    @classmethod
    def fold(cls, block, va, vb):
        if isinstance(va, IrConst):
            if isinstance(vb, IrConst):
                res = cls.fold_const(block, va.width, va.val, vb.val)
                if res is not None:
                    return res
            if cls.commutative:
                return block.make_expr(cls, vb, va)
        if isinstance(vb, IrConst):
            res = cls.fold_const_right(block, va, vb.val)
            if res is not None:
                return res
            if cls.associative and isinstance(va, cls) and isinstance(va.vb, IrConst):
                r = block.make_expr(cls, va.vb, vb)
                return block.make_expr(cls, va.va, r)
        if cls.associative and cls.commutative and isinstance(va, cls) and isinstance(va.vb, IrConst):
            l = block.make_expr(cls, va.va, vb)
            return block.make_expr(cls, l, va.vb)
        if va == vb:
            res = cls.fold_same(block, va)
            if res is not None:
                return res
        return cls.fold_other(block, va, vb)


class IrBinUni(IrBin):
    def __init__(self, block, name, va, vb):
        assert va.width == vb.width
        super().__init__(block, name, va.width, va, vb)


class IrAdd(IrBinUni):
    etype = 'ADD'
    lvl = 7
    symbol = '+'

    commutative = True
    associative = True

    def live_ins(self, mask):
        in_mask = lo_mask(mask)
        return [
            (self.va, in_mask),
            (self.vb, in_mask),
        ]

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(width, a + b)

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va

    @classmethod
    def fold_same(cls, block, va):
        return block.make_expr(IrMul, va, IrConst(va.width, 2))


class IrSub(IrBinUni):
    etype = 'SUB'
    lvl = 7
    symbol = '-'

    def live_ins(self, mask):
        in_mask = lo_mask(mask)
        return [
            (self.va, in_mask),
            (self.vb, in_mask),
        ]

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va
        return block.make_expr(IrAdd, va, IrConst(va.width, -b))

    @classmethod
    def fold_other(cls, block, va, vb):
        if isinstance(va, IrAdd) and isinstance(va.vb, IrConst):
            l = block.make_expr(IrSub, va.va, vb)
            return block.make_expr(IrAdd, l, va.vb)

    @classmethod
    def fold_same(cls, block, va):
        return IrConst(va.width, 0)


class IrMul(IrBinUni):
    etype = 'MUL'
    lvl = 8
    symbol = '*'

    commutative = True
    associative = True

    def live_ins(self, mask):
        in_mask = lo_mask(mask)
        return [
            (self.va, in_mask),
            (self.vb, in_mask),
        ]

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(width, a * b)

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return IrConst(va.width, 0)
        if b == 1:
            return va


class IrUDiv(IrBinUni):
    etype = 'UDIV'
    lvl = 8
    symbol = '/'

    @classmethod
    def fold_const(cls, block, width, a, b):
        if b != 0:
            return IrConst(width, a // b)


class IrSDiv(IrBinUni):
    etype = 'SDIV'
    lvl = 8
    symbol = '/.s'


class IrUMod(IrBinUni):
    etype = 'UMOD'
    lvl = 8
    symbol = '%'

    @classmethod
    def fold_const(cls, block, width, a, b):
        if b != 0:
            return IrConst(width, a % b)
        else:
            return IrConst(width, a)


class IrSMod(IrBinUni):
    etype = 'SMOD'
    lvl = 8
    symbol = '%.s'


class IrBitwise(IrBinUni):
    commutative = True
    associative = True

    def live_ins(self, mask):
        return [
            (self.va, mask),
            (self.vb, mask),
        ]

    @classmethod
    def fold_other(cls, block, va, vb):
        if isinstance(va, IrConcat) or isinstance(vb, IrConcat):
            parts = []
            for lo, hi in cut_ranges(va.width, va.cuts(), vb.cuts()):
                sa = block.make_expr(IrExtr, va, lo, hi - lo)
                sb = block.make_expr(IrExtr, vb, lo, hi - lo)
                parts.append(block.make_expr(cls, sa, sb))
            return block.make_expr(IrConcat, *parts)


class IrAnd(IrBitwise):
    etype = 'AND'
    lvl = 1
    symbol = '&'

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(width, a & b)

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return IrConst(va.width, 0)
        elif b == IrConst(va.width, -1).val:
            return va
        parts = []
        for lo, hi in cut_ranges(va.width, va.cuts(), const_cuts(va.width, b)):
            sa = block.make_expr(IrExtr, va, lo, hi - lo)
            parts.append(block.make_expr(cls, sa, IrConst(hi - lo, b >> lo)))
        return block.make_expr(IrConcat, *parts)

    @classmethod
    def fold_same(cls, block, va):
        return va

    @classmethod
    def fold_ugt(cls, block, va, vb):
        if not isinstance(va, IrXor):
            return
        if va.vb != IrConst(1, 1):
            return
        if not isinstance(va.va, IrUlt):
            return
        exp_vb = block.make_expr(
            IrXor,
            block.make_expr(
                IrEq,
                va.va.va,
                va.va.vb,
            ),
            IrConst(1, 1),
        )
        if vb != exp_vb:
            return
        return block.make_expr(
            IrUlt,
            va.va.vb,
            va.va.va,
        )

    @classmethod
    def fold_other(cls, block, va, vb):
        res = cls.fold_ugt(block, va, vb)
        if res is not None:
            return res
        res = cls.fold_ugt(block, vb, va)
        if res is not None:
            return res


class IrOr(IrBitwise):
    etype = 'OR'
    lvl = 3
    symbol = '|'

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(width, a | b)

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va
        elif b == IrConst(va.width, -1).val:
            return IrConst(va.width, -1)
        parts = []
        for lo, hi in cut_ranges(va.width, va.cuts(), const_cuts(va.width, b)):
            sa = block.make_expr(IrExtr, va, lo, hi - lo)
            parts.append(block.make_expr(cls, sa, IrConst(hi - lo, b >> lo)))
        return block.make_expr(IrConcat, *parts)

    @classmethod
    def fold_same(cls, block, va):
        return va


class IrXor(IrBitwise):
    etype = 'XOR'
    lvl = 2
    symbol = '^'

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(width, a ^ b)

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va

    @classmethod
    def fold_same(cls, block, va):
        return IrConst(va.width, 0)


class IrBinLeft(IrBin):
    def __init__(self, block, name, va, vb):
        super().__init__(block, name, va.width, va, vb)


class IrShl(IrBinLeft):
    etype = 'SHL'
    lvl = 6
    symbol = '<<'

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va
        if b >= va.width:
            return IrConst(va.width, 0)
        return block.make_expr(
            IrConcat,
            IrConst(b, 0),
            block.make_expr(IrExtr, va, 0, va.width - b),
        )


class IrShr(IrBinLeft):
    etype = 'SHR'
    lvl = 6
    symbol = '>>'

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va
        if b >= va.width:
            return IrConst(va.width, 0)
        return block.make_expr(
            IrConcat,
            block.make_expr(IrExtr, va, b, va.width - b),
            IrConst(b, 0),
        )


class IrSar(IrBinLeft):
    etype = 'SAR'
    lvl = 6
    symbol = '>>.s'

    @classmethod
    def fold_const_right(cls, block, va, b):
        if b == 0:
            return va
        if b >= va.width:
            return IrConst(va.width, 0)
        return block.make_expr(
            IrSext,
            block.make_expr(IrExtr, va, b, va.width - b),
            va.width,
        )


class IrBinPred(IrBin):
    def __init__(self, block, name, va, vb):
        assert va.width == vb.width
        super().__init__(block, name, 1, va, vb)


class IrEq(IrBinPred):
    etype = 'EQ'
    lvl = 4
    symbol = '=='

    commutative = True

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(1, int(a == b))

    @classmethod
    def fold_const_right(cls, block, va, b):
        if isinstance(va, IrAdd) and isinstance(va.vb, IrConst):
            return block.make_expr(IrEq, va.va, IrConst(va.width, b - va.vb.val))
        if isinstance(va, IrSub) and b == 0:
            return block.make_expr(IrEq, va.va, va.vb)

    @classmethod
    def fold_same(cls, block, va):
        return IrConst(1, 1)

    @classmethod
    def fold_other(cls, block, va, vb):
        if isinstance(va, IrConcat) or isinstance(vb, IrConcat):
            res = IrConst(1, 1)
            for lo, hi in cut_ranges(va.width, va.cuts(), vb.cuts()):
                sa = block.make_expr(IrExtr, va, lo, hi - lo)
                sb = block.make_expr(IrExtr, vb, lo, hi - lo)
                eq = block.make_expr(IrEq, sa, sb)
                res = block.make_expr(IrAnd, res, eq)
            return res


class IrUlt(IrBinPred):
    etype = 'ULT'
    lvl = 5
    symbol = '<'

    @classmethod
    def fold_const(cls, block, width, a, b):
        return IrConst(1, int(a < b))

    @classmethod
    def fold_same(cls, block, va):
        return IrConst(1, 0)


class IrSlt(IrBinPred):
    etype = 'SLT'
    lvl = 5
    symbol = '<.s'

    @classmethod
    def fold_const(cls, block, width, a, b):
        if a & 1 << (width - 1):
            a |= -1 << width
        if b & 1 << (width - 1):
            b |= -1 << width
        return IrConst(1, int(a < b))

    @classmethod
    def fold_same(cls, block, va):
        return IrConst(1, 0)


class IrAddX(IrExpr):
    etype = 'ADDX'

    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width
        assert vc.width == 1
        super().__init__(block, name, va.width)
        self.va = va
        self.vb = vb
        self.vc = vc

    def live_ins(self, mask):
        in_mask = lo_mask(mask)
        return [
            (self.va, in_mask),
            (self.vb, in_mask),
            (self.vc, 1)
        ]

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
    etype = 'CF'

    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width == vc.width == 1
        super().__init__(block, name, 1)
        self.va = va
        self.vb = vb
        self.vc = vc

    def live_ins(self, mask):
        return [
            (self.va, 1),
            (self.vb, 1),
            (self.vc, 1),
        ]

    def display(self):
        return '{} = CF({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)

    @classmethod
    def fold_ult(cls, block, va, vb, vc):
        if not isinstance(vc, IrExtr):
            return
        width = vc.pos + 1
        if not isinstance(vc.va, IrSub):
            return
        exp_va = block.make_expr(IrExtr, vc.va.va, vc.pos, 1)
        exp_vb = block.make_expr(
            IrXor,
            block.make_expr(IrExtr, vc.va.vb, vc.pos, 1),
            IrConst(1, 1)
        )
        if va != exp_va:
            return
        if vb != exp_vb:
            return
        return block.make_expr(
            IrXor,
            block.make_expr(
                IrUlt,
                block.make_expr(
                    IrExtr,
                    vc.va.va,
                    0,
                    width
                ),
                block.make_expr(
                    IrExtr,
                    vc.va.vb,
                    0,
                    width
                ),
            ),
            IrConst(1, 1),
        )

    @classmethod
    def fold_ult_const(cls, block, va, vb, vc):
        if not isinstance(vc, IrExtr):
            return
        width = vc.pos + 1
        if not isinstance(vc.va, IrAdd):
            return
        if not isinstance(vc.va.vb, IrConst):
            return
        exp_va = block.make_expr(IrExtr, vc.va.va, vc.pos, 1)
        neg_vb = block.make_expr(
            IrSub,
            IrConst(vc.va.vb.width, 0),
            vc.va.vb
        )
        exp_vb = block.make_expr(
            IrXor,
            block.make_expr(IrExtr, neg_vb, vc.pos, 1),
            IrConst(1, 1)
        )
        if va != exp_va:
            return
        if vb != exp_vb:
            return
        return block.make_expr(
            IrXor,
            block.make_expr(
                IrUlt,
                block.make_expr(
                    IrExtr,
                    vc.va.va,
                    0,
                    width
                ),
                block.make_expr(
                    IrExtr,
                    neg_vb,
                    0,
                    width
                ),
            ),
            IrConst(1, 1),
        )

    @classmethod
    def fold(cls, block, va, vb, vc):
        if isinstance(va, IrConst) and isinstance(vb, IrConst) and isinstance(vc, IrConst):
            if va.val == 0 and vb.val == 0:
                res = 0
            elif va.val == 1 and vb.val == 1:
                res = 1
            else:
                res = vc.val ^ 1
            return IrConst(1, res)
        res = cls.fold_ult(block, va, vb, vc)
        if res is not None:
            return res
        res = cls.fold_ult_const(block, va, vb, vc)
        if res is not None:
            return res


class IrOF(IrExpr):
    etype = 'OF'

    def __init__(self, block, name, va, vb, vc):
        assert va.width == vb.width == vc.width == 1
        super().__init__(block, name, 1)
        self.va = va
        self.vb = vb
        self.vc = vc

    def live_ins(self, mask):
        return [
            (self.va, 1),
            (self.vb, 1),
            (self.vc, 1),
        ]

    def display(self):
        return '{} = OF({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)

    @classmethod
    def fold(cls, block, va, vb, vc):
        if isinstance(va, IrConst) and isinstance(vb, IrConst) and isinstance(vc, IrConst):
            return IrConst(1, int(va.val == vb.val and va.val != vc.val))
        if va == vc or vb == vc:
            return IrConst(1, 0)


class CPart:
    def __init__(self, va, shift, mask, sext=None):
        self.va = va
        self.shift = shift
        self.mask = mask
        self.sext = sext


class IrConcat(IrExpr):
    etype = 'CONCAT'

    def __init__(self, block, name, *parts):
        super().__init__(block, name, sum(part.width for part in parts))
        self.parts = parts
        self.cparts = []
        self.cpart_const = 0
        pos = 0
        cpart_cache = {}
        for part in parts:
            if isinstance(part, IrConst):
                self.cpart_const |= part.val << pos
            else:
                mask = ((1 << part.width) - 1) << pos
                sext = None
                val = part
                if isinstance(val, IrSext):
                    val = val.va
                    sext = val.width
                if isinstance(val, IrExtr):
                    shift = pos - val.pos
                    if sext is not None:
                        sext += val.pos
                    val = val.va
                else:
                    shift = pos
                if (val, shift) in cpart_cache:
                    cpart = cpart_cache[val, shift]
                    cpart.mask |= mask
                    if sext is not None:
                        cpart.sext = sext
                        del cpart_cache[val, shift]
                else:
                    cpart = CPart(val, shift, mask, sext)
                    self.cparts.append(cpart)
                    if sext is None:
                        cpart_cache[val, shift] = cpart
            pos += part.width
        for cpart in self.cparts:
            max_mask = ((1 << self.width) - 1)
            if cpart.shift > 0:
                max_mask &= max_mask << cpart.shift
            else:
                max_mask >>= -cpart.shift
            if ((cpart.mask | self.cpart_const) & max_mask) == max_mask:
                cpart.mask = None

    def live_ins(self, mask):
        pos = 0
        res = []
        for part in self.parts:
            res.append((part, mask >> pos))
            pos += part.width
        return res

    def display(self):
        return '{} = CONCAT({})'.format(self.name, ', '.join(str(x) for x in self.parts))

    def cuts(self):
        pos = 0
        res = set()
        for part in self.parts:
            if pos != 0:
                res.add(pos)
            pos += part.width
        return res

    @classmethod
    def fold(cls, block, *parts):
        if len(parts) == 1:
            return parts[0]
        new_parts = []
        folded = False
        for part in parts:
            if new_parts and isinstance(part, IrConst) and isinstance(new_parts[-1], IrConst):
                lo = new_parts[-1]
                new_parts[-1] = IrConst(lo.width + part.width, lo.val | part.val << lo.width)
                folded = True
            elif isinstance(part, IrConcat):
                new_parts += part.parts
                folded = True
            else:
                new_parts.append(part)
        if folded:
            return block.make_expr(IrConcat, *new_parts)


class IrExtr(IrExpr):
    etype = 'EXTR'

    def __init__(self, block, name, va, pos, width):
        super().__init__(block, name, width)
        self.va = va
        self.pos = pos

    def live_ins(self, mask):
        return [
            (self.va, mask << self.pos),
        ]

    def display(self):
        return '{} = EXTR({}, {}, {})'.format(self.name, self.va, self.pos, self.width)

    @classmethod
    def fold(cls, block, va, pos, width):
        assert width + pos <= va.width
        if pos == 0 and width == va.width:
            return va
        if isinstance(va, IrConst):
            return IrConst(width, va.val >> pos)
        if isinstance(va, IrConcat):
            start = 0
            new_parts = []
            my_end = pos + width
            for part in va.parts:
                end = start + part.width
                if end <= pos:
                    start = end
                    continue
                if my_end <= start:
                    start = end
                    continue
                if start < pos:
                    diff = pos - start
                    part = block.make_expr(IrExtr, part, diff, part.width - diff)
                    start += diff
                if my_end < end:
                    part = block.make_expr(IrExtr, part, 0, my_end - start)
                new_parts.append(part)
                start = end
            assert sum(part.width for part in new_parts) == width
            return block.make_expr(IrConcat, *new_parts)
        if isinstance(va, IrExtr):
            return block.make_expr(IrExtr, va.va, pos + va.pos, width)
        if isinstance(va, IrSext):
            if width + pos <= va.va.width:
                return block.make_expr(IrExtr, va.va, pos, width)
            elif pos < va.va.width:
                return block.make_expr(IrSext,
                    block.make_expr(IrExtr, va.va, pos, va.va.width - pos),
                    width
                )
            else:
                return block.make_expr(IrSext,
                    block.make_expr(IrExtr, va.va, va.va.width - 1, 1),
                    width
                )
        if isinstance(va, (IrAnd, IrOr, IrXor)):
            a = block.make_expr(IrExtr, va.va, pos, width)
            b = block.make_expr(IrExtr, va.vb, pos, width)
            return block.make_expr(type(va), a, b)
        if isinstance(va, IrSlct):
            b = block.make_expr(IrExtr, va.vb, pos, width)
            c = block.make_expr(IrExtr, va.vc, pos, width)
            return block.make_expr(IrSlct, va.va, b, c)
        if isinstance(va, (IrAdd, IrSub, IrMul, IrAddX)) and width + pos != va.width:
            a = block.make_expr(IrExtr, va.va, 0, pos + width)
            b = block.make_expr(IrExtr, va.vb, 0, pos + width)
            if isinstance(va, IrAddX):
                c = block.make_expr(IrAddX, a, b, va.vc)
            else:
                c = block.make_expr(type(va), a, b)
            return block.make_expr(IrExtr, c, pos, width)


class IrSext(IrExpr):
    etype = 'SEXT'

    def __init__(self, block, name, va, width):
        super().__init__(block, name, width)
        self.va = va

    def live_ins(self, mask):
        return [
            (self.va, mask),
        ]

    def display(self):
        return '{} = SEXT({}, {})'.format(self.name, self.va, self.width)

    @classmethod
    def fold(cls, block, va, width):
        assert width >= va.width
        if width == va.width:
            return va
        if isinstance(va, IrConst):
            val = va.val
            if val & 1 << (va.width - 1):
                val |= -1 << va.width
            return IrConst(width, val)
        if isinstance(va, IrConcat):
            final = va.parts[-1]
            final = block.make_expr(IrSext, final, width - (va.width - final.width))
            parts = va.parts[:-1] + [final]
            assert sum(part.width for part in parts) == width
            return block.make_expr(IrConcat, *parts)


class IrSlct(IrExpr):
    etype = 'SLCT'

    def __init__(self, block, name, va, vb, vc):
        assert va.width == 1
        assert vb.width == vc.width
        super().__init__(block, name, vb.width)
        self.va = va
        self.vb = vb
        self.vc = vc

    def live_ins(self, mask):
        return [
            (self.va, 1),
            (self.vb, mask),
            (self.vc, mask),
        ]

    def display(self):
        return '{} = SLCT({}, {}, {})'.format(self.name, self.va, self.vb, self.vc)

    @classmethod
    def fold(cls, block, va, vb, vc):
        if isinstance(va, IrXor) and isinstance(va.vb, IrConst):
            assert va.vb.val == 1
            return block.make_expr(IrSlct, va.va, vc, vb)
        if isinstance(va, IrConst):
            assert va.width == 1
            if va.val:
                return vb
            else:
                return vc
        if vb == vc:
            return vb
        if isinstance(vb, IrSlct) and vb.va == va:
            return block.make_expr(IrSlct, va, vb.vb, vc)
        if isinstance(vc, IrSlct) and vc.va == va:
            return block.make_expr(IrSlct, va, vb, vc.vc)


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
    def __init__(self, block, special, ins, extra):
        super().__init__(block)
        self.special = special
        self.ins = list(ins)
        self.extra = extra


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


class IrCallReturn:
    def __init__(self, finish, results):
        self.finish = finish
        self.results = results


class IrCall(IrFinish):
    def __init__(self, block, tree, extra, returns):
        super().__init__(block)
        self.tree = tree
        self.extra = extra
        self.returns = returns


class IrReturn(IrFinish):
    def __init__(self, block, path, extra):
        super().__init__(block)
        self.path = path
        self.extra = extra
