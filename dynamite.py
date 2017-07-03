#!/usr/bin/env python3

# And we all
# Die, die, die tonight
# Sanctified with dynamite
# Die, die, dynamite
# Halleluja!

import argparse

parser = argparse.ArgumentParser(description='Structuralize a list of basic blocks.', conflict_handler='resolve')
parser.add_argument('file', help='the input file')
parser.add_argument('-d', '--print-domtree', action='store_true', help='print the dominator tree')
parser.add_argument('-D', '--debug-domtree', action='store_true', help='debug the dominator tree construction phase')
parser.add_argument('-h', '--print-halfstruct', action='store_true', help='print the half-structures')
parser.add_argument('-H', '--debug-halfstruct', action='store_true', help='debug the half-structuralization phase')
parser.add_argument('-F', '--debug-final', action='store_true', help='debug the final phase')
args = parser.parse_args()

WEIGHT_LIGHT = 0
WEIGHT_NORMAL = 1
WEIGHT_HEAVY = 2

class Block:
    pass

class UncondBlock:
    def __init__(self, out, weight):
        self.out = out
        self.weight = weight

    def outs(self):
        return {self.out}

    def __str__(self):
        return '-> {}'.format(self.out)

    def subst_out(self, old, new):
        assert old == self.out
        self.out = new

class CondBlock:
    def __init__(self, cond, outp, outn, weight):
        self.cond = cond
        self.outp = outp
        self.outn = outn
        self.weight = weight

    def outs(self):
        return {self.outp, self.outn}

    def __str__(self):
        return '-> {} ? {} : {}'.format(self.cond, self.outp, self.outn)

    def subst_out(self, old, new):
        if old == self.outp:
            self.outp = new
        elif old == self.outn:
            self.outn = new
        else:
            assert False

class Case:
    def __init__(self, value, out):
        self.value = value
        self.out = out

class SwitchBlock:
    def __init__(self, expr, cases, outd, weight):
        self.expr = expr
        self.cases = cases
        self.outd = outd
        self.weight = weight

    def outs(self):
        res = {case.out for case in cases}
        if self.outd is not None:
            res.add(self.outd)
        return res

    def __str__(self):
        return '-> {} switch {} default {}'.format(self.expr, ', '.join('{}: {}'.format(x.value, x.out) for x in self.cases), self.outd)

    def subst_out(self, old, new):
        found = False
        if old == self.outd:
            self.outd = new
            found = True
        for c in self.cases:
            if old == c.out:
                c.out = new
                found = True
        assert found

class EndBlock:
    def __init__(self, weight):
        self.weight = weight

    def outs(self):
        return set()

    def __str__(self):
        return '-> /'

    def subst_out(self, old, new):
        assert False

class ReturnBlock:
    def __init__(self, weight):
        self.weight = weight

    def outs(self):
        return set()

    def __str__(self):
        return '-> RETURN'

    def subst_out(self, old, new):
        assert False

# PHASE 1: READ

blocks = {}
entry = None

with open(args.file) as f:
    for l in f:
        l, _, _ = l.partition('#')
        p = l.split()
        if not p:
            continue
        b = p[0]
        if '->' in b:
            raise ValueError("Block name must not contain ->")
        if b in blocks:
            raise ValueError("Duplicate block {}".format(b))
        if entry is None:
            entry = p[0]
        kind = p[1]
        weight = WEIGHT_NORMAL
        if kind[-1] == '+':
            weight = WEIGHT_HEAVY
            kind = kind[:-1]
        elif kind[-1] == '-':
            weight = WEIGHT_LIGHT
            kind = kind[:-1]
        if kind == 'U':
            if len(p) != 3:
                raise ValueError("U needs 1 output")
            blocks[b] = UncondBlock(p[2], weight)
        elif kind == 'C':
            if len(p) != 5:
                raise ValueError("C needs 3 params")
            blocks[b] = CondBlock(p[2], p[3], p[4], weight)
            if p[3] == p[4]:
                raise ValueError("C needs two different exits")
        elif kind == 'E':
            if len(p) != 2:
                raise ValueError("E needs no params")
            blocks[b] = EndBlock(weight)
        elif kind == 'R':
            if len(p) != 2:
                raise ValueError("R needs no params")
            blocks[b] = ReturnBlock(weight)
        elif kind == 'S':
            if len(p) < 3:
                raise ValueError("S needs at least one param")
            params = p[3:]
            cases = [
                Case(params[2*i], params[2*i+1])
                for i in range(len(params)//2)
            ]
            outd = None
            if len(params) % 2:
                outd = params[-1]
            blocks[b] = SwitchBlock(p[2], cases, outd, weight)
        else:
            raise ValueError("Unknown block type {}".format(p[1]))

# PHASE 2: DOMTREE, CRIT EDGE SPLIT

queue = [(None, entry)]
dom = {}
rdom = {None: set()}
exits = {}
multiin = set()

def disp(nest, block):
    indent = nest * '  '
    print('{}{} {}; EXITS: {}; DOM: {}'.format(indent, block, blocks[block], ', '.join('{}[{}]'.format(k, v) if v != block else k for k, v in exits[block].items()), dom[block]))
    for c in rdom[block]:
        disp(nest+1, c)

def lca(a, b):
    # root
    if a is None or b is None:
        return None
    cand = set()
    while True:
        cand.add(a)
        cand.add(b)
        if dom[a] is not None:
            a = dom[a]
            if a in cand:
                return a
        if dom[b] is not None:
            b = dom[b]
            if b in cand:
                return b
        if a == b:
            return a

while queue:
    if args.debug_domtree:
        print('-' * 10 + ' DOMTREE ITERATION ' + '-' * 10)
    cur_from, cur_to = queue.pop()
    if cur_to not in dom:
        if args.debug_domtree:
            print('NEW', cur_from, cur_to)
        dom[cur_to] = cur_from
        exits[cur_to] = {}
        rdom[cur_to] = set()
        rdom[cur_from].add(cur_to)
        if cur_to not in blocks:
            raise ValueError("Unknown block {}".format(cur_to))
        for out in blocks[cur_to].outs():
            if args.debug_domtree:
                print('QUEUE', cur_to, out)
            queue.append((cur_to, out))
    else:
        if args.debug_domtree:
            print('CONSIDER', dom[cur_to], cur_from, cur_to, cur_to in multiin)
        if cur_to not in multiin:
            if dom[cur_to] is None:
                transit = '->' + cur_to
                blocks[transit] = UncondBlock(cur_to, WEIGHT_LIGHT)
                dom[transit] = None
                rdom[transit] = {cur_to}
                exits[transit] = dict(exits[cur_to])
                assert entry == cur_to
                entry = transit
                rdom[None].remove(cur_to)
                rdom[None].add(transit)
                dom[cur_to] = transit
            else:
                parent = blocks[dom[cur_to]]
                if not isinstance(parent, UncondBlock):
                    if args.debug_domtree:
                        print('POSTSPLIT', cur_from, dom[cur_to], cur_to)
                    transit = dom[cur_to] + '->' + cur_to
                    blocks[transit] = UncondBlock(cur_to, WEIGHT_LIGHT)
                    dom[transit] = dom[cur_to]
                    rdom[transit] = {cur_to}
                    exits[transit] = dict(exits[cur_to])
                    parent.subst_out(cur_to, transit)
                    rdom[dom[cur_to]].remove(cur_to)
                    rdom[dom[cur_to]].add(transit)
                    dom[cur_to] = transit
            multiin.add(cur_to)
        if cur_from is not None and cur_to in blocks[cur_from].outs() and not isinstance(blocks[cur_from], UncondBlock):
            if args.debug_domtree:
                print('PRESPLIT', dom[cur_to], cur_from, cur_to)
            transit = cur_from + '->' + cur_to
            blocks[transit] = UncondBlock(cur_to, WEIGHT_LIGHT)
            blocks[cur_from].subst_out(cur_to, transit)
            dom[transit] = cur_from
            exits[transit] = {}
            rdom[transit] = set()
            rdom[cur_from].add(transit)
            cur_from = transit
        cur_dom = dom[cur_to]
        new_dom = lca(cur_dom, cur_from)
        if cur_dom != new_dom:
            rdom[cur_dom].remove(cur_to)
            while cur_dom != new_dom:
                new_outs = {x : cur_dom for x in blocks[cur_dom].outs() if x not in rdom[cur_dom]}
                for c in rdom[cur_dom]:
                    for new_out, out_dom in exits[c].items():
                        if new_out in rdom[cur_dom]:
                            continue
                        if new_out in new_outs:
                            new_outs[new_out] = lca(out_dom, new_outs[new_out])
                        else:
                            new_outs[new_out] = out_dom
                exits[cur_dom] = new_outs
                cur_dom = dom[cur_dom]
            dom[cur_to] = new_dom
            rdom[new_dom].add(cur_to)
            for x in exits[cur_to]:
                if args.debug_domtree:
                    print('REQUEUE', cur_to, x)
                queue.append((cur_to, x))
        orig_from = cur_from
        while cur_from != new_dom:
            exits[cur_from][cur_to] = orig_from
            cur_from = dom[cur_from]
    if args.debug_domtree:
        disp(0, entry)
        print('INFLIGHT', queue)

if args.print_domtree or args.debug_domtree:
    print('-' * 20 + ' DOMINATOR TREE ' + '-' * 20)
    disp(0, entry)

# PHASE 3: HALF-STRUCTURALIZE

struct = {}

class ExprThen:
    def __new__(cls, expra, exprb):
        if isinstance(expra, ExprVoid):
            return exprb
        self = super().__new__(cls)
        self.expra = expra
        assert expra.arity() == 1
        self.exprb = exprb
        self._arity = exprb.arity()
        return self

    def arity(self):
        return self._arity

    def __str__(self):
        return '{}, {}'.format(self.expra, self.exprb)


class ExprLeafZero:
    def __init__(self, block):
        self.block = block

    def arity(self):
        return 0

    def __str__(self):
        return '{}#'.format(self.block)


class ExprLeafOne:
    def __init__(self, block):
        self.block = block

    def arity(self):
        return 1

    def __str__(self):
        return self.block


class ExprLeafCond:
    def __init__(self, cond):
        self.cond = cond

    def arity(self):
        return 2

    def __str__(self):
        return self.cond


class ExprConstBool:
    def __init__(self, which):
        self.which = which

    def arity(self):
        return 2

    def __str__(self):
        return 'true' if self.which else 'false'


class ExprCond:
    def __new__(cls, exprc, exprp, exprn):
        if isinstance(exprc, ExprThen):
            return ExprThen(exprc.expra, ExprCond(exprc.exprb, exprp, exprn))
        if isinstance(exprp, ExprConstBool) and isinstance(exprn, ExprConstBool):
            if exprp.which == 1 and exprn.which == 0:
                return exprc
            elif exprp.which == 0 and exprn.which == 1:
                return ExprNot(exprc)
        self = super().__new__(cls)
        self.exprc = exprc
        self.exprp = exprp
        self.exprn = exprn
        assert exprc.arity() == 2
        self._arity = exprp.arity() | exprn.arity()
        assert self._arity in (0, 1, 2)
        return self

    def arity(self):
        return self._arity

    def __str__(self):
        if isinstance(self.exprn, ExprConstBool) and self.exprn.which == 0:
            return '({}) && ({})'.format(self.exprc, self.exprp)
        if isinstance(self.exprn, ExprConstBool) and self.exprn.which == 1:
            return '!({}) || ({})'.format(self.exprc, self.exprp)
        if isinstance(self.exprp, ExprConstBool) and self.exprp.which == 1:
            return '({}) || ({})'.format(self.exprc, self.exprn)
        if isinstance(self.exprp, ExprConstBool) and self.exprp.which == 0:
            return '!({}) && ({})'.format(self.exprc, self.exprn)
        return '({}) ? ({}) : ({})'.format(self.exprc, self.exprp, self.exprn)


class ExprNot:
    def __new__(cls, expr):
        assert expr.arity() == 2
        if isinstance(expr, ExprThen):
            return ExprThen(expr.expra, ExprNot(expr.exprb))
        if isinstance(expr, ExprCond):
            return ExprCond(expr.exprc, ExprNot(expr.expra), ExprNot(expr.exprb))
        self = super().__new__(cls)
        self.expr = expr
        return self

    def arity(self):
        return 2

    def __str__(self):
        return '!({})'.format(self.expr)


class ExprVoid:
    def arity(self):
        return 1

    def __str__(self):
        return 'void'


# 0 pure
class StructExprZ:
    def __init__(self, expr):
        assert expr.arity() == 0
        self.expr = expr

    def exits(self):
        return set()

# 1 pure
class StructExprE:
    def __init__(self, expr, exit):
        assert expr.arity() == 1
        self.expr = expr
        self.exit = exit

    def exits(self):
        return {self.exit}

class StructExprD:
    def __init__(self, expr, stmt):
        assert expr.arity() == 1
        self.expr = expr
        self.stmt = stmt
        self._exits = stmt.exits()

    def exits(self):
        return self._exits

class StructHeavy:
    def __init__(self, stmt):
        self.stmt = stmt
        self._exits = stmt.exits()

    def exits(self):
        return self._exits

class StructExprDD:
    def __init__(self, expr, stmtp, stmtn, joins):
        assert expr.arity() == 2
        self.expr = expr
        self.stmtp = stmtp
        self.stmtn = stmtn
        self.joins = joins
        self._exits = stmtp.exits() | stmtn.exits()
        for stmt in self.joins.values():
            self._exits |= stmt.exits()
        self._exits -= set(self.joins)

    def exits(self):
        return self._exits

class StructSwitch:
    def __init__(self, block, expr, cases, outd, joins):
        self.block = block
        self.expr = expr
        self.cases = cases
        self.outd = outd
        self.joins = joins
        self._exits = set()
        for stmt in self.joins.values():
            self._exits |= stmt.exits()
        self._exits -= set(self.joins)

    def exits(self):
        return self._exits

class StructLoop:
    def __init__(self, block, stmt):
        self.block = block
        self.stmt = stmt
        self._exits = stmt.exits() - {block}

    def exits(self):
        return self._exits

class StructReturn:
    def __init__(self, block):
        self.block = block

    def exits(self):
        return set()

def sdispe(nest, e):
    indent = nest * '  '
    assert e.arity() in {0, 1}
    if isinstance(e, ExprLeafOne):
        print('{}{}'.format(indent, e.block))
    elif isinstance(e, ExprLeafZero):
        print('{}{}'.format(indent, e.block))
        print('{}# unreachable'.format(indent))
    elif isinstance(e, ExprThen):
        sdispe(nest, e.expra)
        sdispe(nest, e.exprb)
    elif isinstance(e, ExprVoid):
        pass
    elif isinstance(e, ExprCond):
        print('{}if ({}) {{'.format(indent, e.exprc))
        sdispe(nest+1, e.exprp)
        if not isinstance(e.exprn, ExprVoid):
            print('{}}} else {{'.format(indent))
            sdispe(nest+1, e.exprn)
        print('{}}}'.format(indent))
    else:
        print('{}{} # {}'.format(indent, e, type(e)))
        assert 0

def sdisps(nest, s):
    indent = nest * '  '
    if isinstance(s, StructExprZ):
        print('{}// Z'.format(indent))
        sdispe(nest, s.expr)
    elif isinstance(s, StructExprE):
        print('{}// E'.format(indent))
        sdispe(nest, s.expr)
        print('{}goto {}'.format(indent, s.exit))
    elif isinstance(s, StructExprD):
        print('{}// D'.format(indent))
        sdispe(nest, s.expr)
        sdisps(nest, s.stmt)
    elif isinstance(s, StructExprDD):
        cond = s.expr
        while isinstance(cond, ExprThen):
            sdispe(nest, cond.expra)
            cond = cond.exprb
        print('{}if ({}) {{ // DD'.format(indent, cond))
        sdisps(nest+1, s.stmtp)
        print('{}}} else {{'.format(indent))
        sdisps(nest+1, s.stmtn)
        print('{}}}'.format(indent))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
    elif isinstance(s, StructReturn):
        print('{}// R'.format(indent))
        if blocks[s.block].weight != WEIGHT_LIGHT:
            print('{}{}'.format(indent, s.block))
        print('{}return'.format(indent))
    elif isinstance(s, StructSwitch):
        if blocks[s.block].weight != WEIGHT_LIGHT:
            print('{}{}'.format(indent, s.block))
        print('{}switch ({}) {{'.format(indent, s.expr))
        for c in s.cases:
            print('{}case {}:'.format(indent, c.value))
            print('{}  goto {}'.format(indent, c.out))
        if s.outd is not None:
            print('{}default:'.format(indent))
            print('{}  goto {}'.format(indent, s.outd))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
        print('{}}}'.format(indent))
    elif isinstance(s, StructLoop):
        print('{}{}: while (1) {{'.format(indent, s.block))
        sdisps(nest+1, s.stmt)
        print('{}}}'.format(indent))
    elif isinstance(s, StructHeavy):
        print('{}// H'.format(indent))
        sdisps(nest, s.stmt)
    else:
        print('{}{}'.format(indent, type(s)))
        assert 0

if args.debug_halfstruct:
    print('-' * 10 + ' START HALF-STRUCTURALIZATION ' + '-' * 10)

def is_e(s):
    return isinstance(s, StructExprE)

def is_ee(s):
    return isinstance(s, StructExprDD) and is_e(s.stmtp) and is_e(s.stmtn)

def is_de(s):
    return isinstance(s, StructExprDD) and (is_e(s.stmtp) or is_e(s.stmtn))

def simplify(struct):
    if args.debug_halfstruct:
        print("STARTING:")
        sdisps(1, struct)
    while True:
        if isinstance(struct, StructExprD):
            child = struct.stmt
            if isinstance(child, StructExprZ):
                struct = StructExprZ(
                    ExprThen(struct.expr, child.expr)
                )
            elif isinstance(child, StructExprE):
                struct = StructExprE(
                    ExprThen(struct.expr, child.expr),
                    child.exit
                )
            elif isinstance(child, StructExprD):
                struct = StructExprD(
                    ExprThen(struct.expr, child.expr),
                child.stmt)
            elif isinstance(child, StructExprDD):
                struct = StructExprDD(
                    ExprThen(struct.expr, child.expr),
                    child.stmtp,
                    child.stmtn,
                    child.joins,
                )
            else:
                return struct
        elif isinstance(struct, StructExprDD):
            if (isinstance(struct.stmtp, StructExprE) and
                struct.stmtp.exit in struct.joins and
                struct.stmtp.exit not in struct.stmtn.exits() and
                all(struct.stmtp.exit not in join.exits() for join in struct.joins.values())):
                struct = StructExprDD(
                    struct.expr,
                    simplify(StructExprD(struct.stmtp.expr, struct.joins[struct.stmtp.exit])),
                    struct.stmtn,
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.stmtp.exit
                    }
                )
            elif (isinstance(struct.stmtn, StructExprE) and
                struct.stmtn.exit in struct.joins and
                struct.stmtn.exit not in struct.stmtp.exits() and
                all(struct.stmtn.exit not in join.exits() for join in struct.joins.values())):
                struct = StructExprDD(
                    struct.expr,
                    struct.stmtp,
                    simplify(StructExprD(struct.stmtn.expr, struct.joins[struct.stmtn.exit])),
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.stmtn.exit
                    }
                )
            elif isinstance(struct.stmtp, StructExprZ) and isinstance(struct.stmtn, StructExprZ):
                assert not struct.joins
                struct = StructExprZ(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        struct.stmtn.expr,
                    ),
                )
            elif isinstance(struct.stmtp, StructExprZ):
                assert not struct.joins
                struct = StructExprD(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        ExprVoid()
                    ),
                    struct.stmtn
                )
            elif isinstance(struct.stmtn, StructExprZ):
                assert not struct.joins
                struct = StructExprD(
                    ExprCond(
                        struct.expr,
                        ExprVoid(),
                        struct.stmtn.expr
                    ),
                    struct.stmtp
                )
            elif is_ee(struct):
                if struct.stmtp.exit != struct.stmtn.exit:
                    return struct
                elif struct.stmtp.exit in struct.joins:
                    assert len(struct.joins) == 1
                    struct = StructExprD(
                        ExprCond(
                            struct.expr,
                            struct.stmtp.expr,
                            struct.stmtn.expr
                        ),
                        struct.joins[struct.stmtp.exit]
                    )
                else:
                    assert not struct.joins
                    struct = StructExprE(
                        ExprCond(
                            struct.expr,
                            stmtp.expr,
                            stmtn.expr,
                        ),
                        stmtp.exit
                    )
            elif (is_e(struct.stmtp) and is_de(struct.stmtn)) or (is_de(struct.stmtp) and is_e(struct.stmtn)):
                if is_e(struct.stmtp):
                    e = struct.stmtp
                    de = struct.stmtn
                    neg = True
                else:
                    e = struct.stmtn
                    de = struct.stmtp
                    neg = False
                assert is_de(de)
                assert is_e(e)
                if is_e(de.stmtp) and de.stmtp.exit == e.exit:
                    assert not de.joins
                    cond_de = ExprCond(de.expr, ExprThen(de.stmtp.expr, ExprConstBool(1)), ExprConstBool(0))
                    cond_e = ExprThen(e.expr, ExprConstBool(1))
                    stmtp = StructExprE(ExprVoid(), e.exit)
                    stmtn = de.stmtn
                elif is_e(de.stmtn) and de.stmtn.exit == e.exit:
                    assert not de.joins
                    cond_de = ExprCond(de.expr, ExprConstBool(1), ExprThen(de.stmtn.expr, ExprConstBool(0)))
                    cond_e = ExprThen(e.expr, ExprConstBool(0))
                    stmtp = de.stmtp
                    stmtn = StructExprE(ExprVoid(), e.exit)
                else:
                    return struct
                if neg:
                    cond = ExprCond(
                        struct.expr,
                        cond_e,
                        cond_de,
                    )
                else:
                    cond = ExprCond(
                        struct.expr,
                        cond_de,
                        cond_e,
                    )
                struct = StructExprDD(
                    cond,
                    stmtp,
                    stmtn,
                    struct.joins
                )
            elif is_ee(struct.stmtp) and is_ee(struct.stmtn):
                condp = ExprCond(
                    struct.stmtp.expr,
                    ExprThen(struct.stmtp.stmtp.expr, ExprConstBool(1)),
                    ExprThen(struct.stmtp.stmtn.expr, ExprConstBool(0)),
                )
                exitp = struct.stmtp.stmtp.exit
                exitn = struct.stmtp.stmtn.exit
                if struct.stmtn.stmtp.exit == exitp and struct.stmtn.stmtn.exit == exitn:
                    condn = ExprCond(
                        struct.stmtn.expr,
                        ExprThen(struct.stmtn.stmtp.expr, ExprConstBool(1)),
                        ExprThen(struct.stmtn.stmtn.expr, ExprConstBool(0)),
                    )
                elif struct.stmtn.stmtp.exit == exitn and struct.stmtn.stmtn.exit == exitp:
                    condn = ExprCond(
                        struct.stmtn.expr,
                        ExprThen(struct.stmtn.stmtp.expr, ExprConstBool(0)),
                        ExprThen(struct.stmtn.stmtn.expr, ExprConstBool(1)),
                    )
                else:
                    return struct
                struct = StructExprDD(
                    ExprCond(struct.expr, condp, condn),
                    StructExprE(ExprVoid(), exitp),
                    StructExprE(ExprVoid(), exitn),
                    struct.joins
                )
            else:
                return struct
        else:
            return struct
        if args.debug_halfstruct:
            print(" SIMPLIFIED:")
            sdisps(1, struct)

def structify(block):
    for b in rdom[block]:
        structify(b)
    mah_block = blocks[block]
    if isinstance(mah_block, EndBlock):
        res = StructExprZ(ExprLeafZero(block))
    elif isinstance(mah_block, ReturnBlock):
        res = StructReturn(block)
    elif isinstance(mah_block, UncondBlock):
        mah_expr = ExprLeafOne(block)
        if mah_block.weight == WEIGHT_LIGHT:
            mah_expr = ExprVoid()
        if mah_block.out in rdom[block]:
            assert len(rdom[block]) == 1
            res = StructExprD(mah_expr, struct[mah_block.out])
        else:
            assert not rdom[block]
            res = StructExprE(mah_expr, mah_block.out)
    elif isinstance(mah_block, SwitchBlock):
        res = StructSwitch(
            block,
            mah_block.expr,
            mah_block.cases,
            mah_block.outd,
            {
                x: struct[x]
                for x in rdom[block]
            },
        )
    else:
        cond = ExprLeafCond(mah_block.cond)
        if mah_block.weight != WEIGHT_LIGHT:
            cond = ExprThen(ExprLeafOne(block), cond)
        res = StructExprDD(
            cond,
            struct[mah_block.outp],
            struct[mah_block.outn],
            {
                x: struct[x]
                for x in rdom[block] - {mah_block.outp, mah_block.outn}
            },
        )
    res = simplify(res)
    if mah_block.weight == WEIGHT_HEAVY:
        res = StructHeavy(res)
    if block in exits[block]:
        res = StructLoop(block, res)
    struct[block] = res

structify(entry)

if args.print_halfstruct or args.debug_halfstruct:
    print('-' * 20 + ' FINAL HALF-STRUCTURE ' + '-' * 20)
    sdisps(0, struct[entry])

# PHASE 4: FINAL STRUCTURALIZATION

class FinalExpr:
    def __init__(self, expr):
        self.expr = expr

class FinalGoto:
    def __init__(self, exit):
        self.exit = exit

class FinalBreak:
    def __init__(self, loop):
        self.loop = loop

class FinalContinue:
    def __init__(self, loop):
        self.loop = loop

class FinalIf:
    def __init__(self, expr, stmtp, stmtn):
        self.expr = expr
        self.stmtp = stmtp
        self.stmtn = stmtn

class FinalLabel:
    def __init__(self, label):
        self.label = label

class FinalCase:
    def __init__(self, value):
        self.value = value

class FinalDefault:
    pass

class FinalSwitch:
    def __init__(self, label, expr, stmt):
        self.label = label
        self.expr = expr
        self.stmt = stmt

class FinalWhile:
    def __init__(self, label, expr, stmt, else_):
        self.label = label
        self.expr = expr
        self.stmt = stmt
        self.else_ = else_

def fdisp(nest, stmts):
    indent = nest * '  '
    for s in stmts:
        if isinstance(s, StructExprZ):
            if args.debug_final:
                print('{}// Z'.format(indent))
            sdispe(nest, s.expr)
        elif isinstance(s, StructReturn):
            if args.debug_final:
                print('{}// R'.format(indent))
            print('{}{}'.format(indent, s.block))
            print('{}return'.format(indent))
        elif isinstance(s, FinalExpr):
            if args.debug_final:
                print('{}// E'.format(indent))
            sdispe(nest, s.expr)
        elif isinstance(s, FinalGoto):
            print('{}goto {}'.format(indent, s.exit))
        elif isinstance(s, FinalBreak):
            if s.loop is None:
                print('{}break'.format(indent))
            else:
                print('{}break {}'.format(indent, s.loop))
        elif isinstance(s, FinalContinue):
            if s.loop is None:
                print('{}continue'.format(indent))
            else:
                print('{}continue {}'.format(indent, s.loop))
        elif isinstance(s, FinalIf):
            cond = s.expr
            while isinstance(cond, ExprThen):
                sdispe(nest, cond.expra)
                cond = cond.exprb
            if not s.stmtp:
                print('{}if ({}) {{'.format(indent, ExprNot(cond)))
                fdisp(nest+1, s.stmtn)
                print('{}}}'.format(indent))
            else:
                print('{}if ({}) {{'.format(indent, cond))
                fdisp(nest+1, s.stmtp)
                if s.stmtn:
                    print('{}}} else {{'.format(indent))
                    fdisp(nest+1, s.stmtn)
                print('{}}}'.format(indent))
        elif isinstance(s, FinalLabel):
            if s.label in used_labels:
                print('{}{}:'.format(indent[:-2], s.label))
        elif isinstance(s, FinalCase):
            print('{}case {}:'.format(indent[:-2], s.value))
        elif isinstance(s, FinalDefault):
            print('{}default:'.format(indent[:-2]))
        elif isinstance(s, FinalSwitch):
            if s.label in used_loops:
                print('{}{}: switch ({}) {{'.format(s.label, indent, s.expr))
            else:
                print('{}switch ({}) {{'.format(indent, s.expr))
            fdisp(nest+1, s.stmt)
            print('{}}}'.format(indent))
        elif isinstance(s, FinalWhile):
            if s.label in used_loops:
                print('{}{}: while ({}) {{'.format(s.label, indent, s.expr))
            else:
                print('{}while ({}) {{'.format(indent, s.expr))
            fdisp(nest+1, s.stmt)
            if s.else_:
                print('{}}} else {{{}'.format(indent))
                fdisp(nest+1, s.else_)
            print('{}}}'.format(indent))
        else:
            print('{}{}'.format(indent, type(s)))
            assert 0

used_labels = set()
used_loops = set()

def toposort(joins, after):
    res = []
    done = set()
    def go(x):
        if x in done:
            return
        done.add(x)
        for y in joins[x].exits():
            if y in joins:
                go(y)
        res.append(x)
    for x in joins:
        if after in joins[x].exits():
            go(x)
    for x in joins:
        go(x)
    return res[::-1]

BREAK_JOIN = 0
BREAK_AFTER = 1
BREAK_MULTI = 2
BREAK_EXIT = 3
BREAK_MEH = 4

def find_breaks(struct, level, top_exits, after, multis):
    if isinstance(struct, StructExprD):
        return find_breaks(struct.stmt, level, top_exits, after, multis)
    elif isinstance(struct, StructLoop):
        return find_breaks(struct.stmt, level, top_exits, after, multis)
    elif isinstance(struct, StructExprDD):
        res = []
        for x in struct.joins.values():
            if x.exits() <= top_exits:
                res.append((BREAK_JOIN, level, x))
            else:
                res += find_breaks(x, level+1, top_exits, after, multis)
        for x in [struct.stmtp, struct.stmtn]:
            if x.exits() <= top_exits:
                if after in x.exits():
                    kind = BREAK_AFTER
                elif multis & x.exits():
                    kind = BREAK_MULTI
                elif x.exits():
                    kind = BREAK_EXIT
                else:
                    kind = BREAK_MEH
                res.append((kind, level, x))
            else:
                res += find_breaks(x, level+1, top_exits, after, multis)
        return res
    elif isinstance(struct, StructSwitch):
        res = []
        for x in struct.joins.values():
            if x.exits() <= top_exits:
                outs = {c.out for c in struct.cases}
                if struct.outd is not None:
                    outs.add(struct.outd)
                if x in outs:
                    if after in x.exits():
                        kind = BREAK_AFTER
                    elif multis & x.exits():
                        kind = BREAK_MULTI
                    elif x.exits():
                        kind = BREAK_EXIT
                    else:
                        kind = BREAK_MEH
                    res.append((kind, level, x))
                else:
                    res.append((BREAK_JOIN, level, x))
            else:
                res += find_breaks(x, level+1, top_exits, after, multis)
        return res
    else:
        return []

def replace_break(struct, break_stmt, break_label):
    if struct is break_stmt:
        return StructExprE(ExprVoid(), break_label)
    elif isinstance(struct, StructExprD):
        return StructExprD(
            struct.expr,
            replace_break(struct.stmt, break_stmt, break_label)
        )
    elif isinstance(struct, StructLoop):
        return StructLoop(
            struct.block,
            replace_break(struct.stmt, break_stmt, break_label)
        )
    elif isinstance(struct, StructExprDD):
        return StructExprDD(
            struct.expr,
            replace_break(struct.stmtp, break_stmt, break_label),
            replace_break(struct.stmtn, break_stmt, break_label),
            {
                k: replace_break(v, break_stmt, break_label)
                for k, v in struct.joins.items()
            }
        )
    elif isinstance(struct, StructSwitch):
        return StructSwitch(
            struct.block,
            struct.expr,
            struct.cases,
            struct.outd,
            {
                k: replace_break(v, break_stmt, break_label)
                for k, v in struct.joins.items()
            }
        )
    else:
        return struct

def final_expr(expr):
    if isinstance(expr, ExprVoid):
        return []
    else:
        return [FinalExpr(expr)]

def finalize(struct, after, labels, cur_break, cur_cont):
    if isinstance(struct, StructExprZ):
        return [struct]
    elif isinstance(struct, StructReturn):
        return [struct]
    elif isinstance(struct, StructExprD):
        return (final_expr(struct.expr) +
            finalize(struct.stmt, after, labels, cur_break, cur_cont))
    elif isinstance(struct, StructExprE):
        if after == struct.exit:
            return final_expr(struct.expr)
        else:
            if struct.exit == cur_break:
                goto = FinalBreak(None)
            elif struct.exit == cur_cont:
                goto = FinalContinue(None)
            elif struct.exit in labels:
                cls, loop = labels[struct.exit]
                used_loops.add(loop)
                goto = cls(loop)
            else:
                used_labels.add(struct.exit)
                goto = FinalGoto(struct.exit)
            return final_expr(struct.expr) + [goto]
    elif isinstance(struct, StructExprDD):
        joins = toposort(struct.joins, after)
        joins.append(after)
        stmtp = finalize(struct.stmtp, joins[0], labels, cur_break, cur_cont)
        stmtn = finalize(struct.stmtn, joins[0], labels, cur_break, cur_cont)
        res = [FinalIf(struct.expr, stmtp, stmtn)]
        for idx in range(len(joins)-1):
            res.append(FinalLabel(joins[idx]))
            stmt = struct.joins[joins[idx]]
            res += finalize(stmt, joins[idx+1], labels, cur_break, cur_cont)
        return res
    elif isinstance(struct, StructSwitch):
        joins = toposort(struct.joins, after)
        if args.debug_final:
            print(struct.block, joins)
        rev = {}
        for c in struct.cases:
            if c.out not in rev:
                rev[c.out] = []
            rev[c.out].append(c.value)
        if struct.outd is not None:
            if struct.outd not in rev:
                rev[struct.outd] = []
            rev[struct.outd].append(None)
        stmts = []
        bad = False
        for x in struct.joins[joins[-1]].exits():
            if x in struct.joins:
                bad = True
        ctr = 0
        for x in struct.joins.values():
            if joins[-1] in x.exits():
                ctr += 1
        if bad or ctr < 2:
            joins.append(after)
        mah_break = joins[-1]
        nest_labels = dict(labels)
        mah_label = struct.block + '.switch'
        nest_labels[mah_break] = FinalBreak, mah_label
        for idx in range(len(joins)-1):
            if joins[idx] in rev:
                for y in rev[joins[idx]]:
                    if y is None:
                        stmts.append(FinalDefault())
                    else:
                        stmts.append(FinalCase(y))
            stmts.append(FinalLabel(joins[idx]))
            stmt = struct.joins[joins[idx]]
            stmts += finalize(stmt, joins[idx+1], nest_labels, mah_break, cur_cont)
        if joins[-1] in rev:
            for y in rev[joins[-1]]:
                if y is None:
                    stmts.append(FinalDefault())
                else:
                    stmts.append(FinalCase(y))
        res = []
        if blocks[struct.block].weight != WEIGHT_LIGHT:
            res.append(FinalExpr(ExprLeafOne(struct.block)))
        res.append(FinalSwitch(mah_label, struct.expr, stmts))
        if mah_break != after:
            res.append(FinalLabel(mah_break))
            stmt = struct.joins[mah_break]
            res += finalize(stmt, after, labels, cur_break, cur_cont)
        return res
    elif isinstance(struct, StructLoop):
        cand_breaks = find_breaks(struct.stmt, 0, struct.exits(), after, set(labels))
        mah_break = None
        break_final = []
        mah_stmt = struct.stmt
        mah_label = struct.block + '.loop'
        if args.debug_final:
            print(struct.block, cand_breaks)
        if cand_breaks:
            break_kind = min(x[0] for x in cand_breaks)
            break_stmt = None
            if break_kind == BREAK_JOIN:
                lvl = min(x[1] for x in cand_breaks if x[0] == BREAK_JOIN)
                break_stmt = [x[2] for x in cand_breaks if x[0] == BREAK_JOIN and x[1] == lvl][0]
            else:
                cand_breaks = [x[2] for x in cand_breaks if x[0] == break_kind]
                if len(cand_breaks) == 1 or break_kind == BREAK_MEH:
                    break_stmt = cand_breaks[0]
                elif break_kind == BREAK_AFTER:
                    mah_break = after
                elif break_kind == BREAK_MULTI:
                    exits = set(labels) & card_breaks[0].exits()
                    mah_break = list(exits)[0]
                elif break_kind == BREAK_EXIT:
                    exits = card_breaks[0].exits()
                    mah_break = list(exits)[0]
                else:
                    assert 0
            if break_stmt is None:
                if args.debug_final:
                    print('BREAK EXIT', struct.block, mah_break)
                break_final = finalize(StructExprE(ExprVoid(), mah_break), after, labels, cur_break, cur_cont)
            else:
                # ugh.
                if args.debug_final:
                    print('BREAK H4X', struct.block, break_stmt)
                mah_break = struct.block + '.break'
                mah_stmt = replace_break(mah_stmt, break_stmt, mah_break)
                break_final = finalize(break_stmt, after, labels, cur_break, cur_cont)
        # XXX think about conts...
        mah_cont = struct.block
        nest_labels = dict(labels)
        if mah_break is not None:
            nest_labels[mah_break] = FinalBreak, mah_label
        nest_labels[mah_cont] = FinalContinue, mah_label
        cond = ExprConstBool(1)
        else_ = []
        inner = finalize(mah_stmt, struct.block, nest_labels, mah_break, mah_cont)
        if inner and isinstance(inner[0], FinalIf):
            if inner[0].stmtp and isinstance(inner[0].stmtp[-1], FinalBreak) and inner[0].stmtp[-1].loop is None:
                cond = ExprNot(inner[0].expr)
                else_ = inner[0].stmtp[:-1]
                inner = inner[0].stmtn + inner[1:]
            elif inner[0].stmtn and isinstance(inner[0].stmtn[-1], FinalBreak) and inner[0].stmtn[-1].loop is None:
                cond = inner[0].expr
                else_ = inner[0].stmtn[:-1]
                inner = inner[0].stmtp + inner[1:]
        return [
            FinalWhile(
                mah_label,
                cond,
                inner,
                else_,
            )
        ] + break_final
    elif isinstance(struct, StructHeavy):
        return finalize(struct.stmt, after, labels, cur_break, cur_cont)
    else:
        assert 0

final = finalize(struct[entry], None, {}, None, None)

if args.debug_domtree or args.print_domtree or args.debug_halfstruct or args.print_halfstruct or args.debug_final:
    print('-' * 30 + ' FINAL RESULT ' + '-' * 30)

fdisp(0, final)
