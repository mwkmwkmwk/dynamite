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

class Block:
    pass

class UncondBlock:
    def __init__(self, out):
        self.out = out

    def outs(self):
        return [self.out]

    def __str__(self):
        return '-> {}'.format(self.out)

    def subst_out(self, old, new):
        assert old == self.out
        self.out = new

class CondBlock:
    def __init__(self, cond, outp, outn):
        self.cond = cond
        self.outp = outp
        self.outn = outn

    def outs(self):
        return [self.outp, self.outn]

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
    def __init__(self, expr, cases, outd):
        self.expr = expr
        self.cases = cases
        self.outd = outd

    def outs(self):
        return ([self.outd] if self.outd else []) + [case.out for case in cases]

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
    def outs(self):
        return []

    def __str__(self):
        return '-> /'

    def subst_out(self, old, new):
        assert False

class ReturnBlock:
    def outs(self):
        return []

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
        if p[1] == 'U':
            if len(p) != 3:
                raise ValueError("U needs 1 output")
            blocks[b] = UncondBlock(p[2])
        elif p[1] == 'C':
            if len(p) != 5:
                raise ValueError("C needs 3 params")
            blocks[b] = CondBlock(p[2], p[3], p[4])
        elif p[1] == 'E':
            if len(p) != 2:
                raise ValueError("E needs no params")
            blocks[b] = EndBlock()
        elif p[1] == 'R':
            if len(p) != 2:
                raise ValueError("R needs no params")
            blocks[b] = ReturnBlock()
        elif p[1] == 'S':
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
            blocks[b] = SwitchBlock(p[2], cases, outd)
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
                blocks[transit] = UncondBlock(cur_to)
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
                    blocks[transit] = UncondBlock(cur_to)
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
            blocks[transit] = UncondBlock(cur_to)
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


class ExprLeafTwo:
    def __init__(self, block, cond):
        self.block = block
        self.cond = cond

    def arity(self):
        return 2

    def __str__(self):
        if '-' in self.block:
            return self.cond
        return '{}, {}'.format(self.block, self.cond)


class ExprConstBool:
    def __init__(self, which):
        self.which = which

    def arity(self):
        return 2

    def __str__(self):
        return 'true' if self.which else 'false'


class ExprCond:
    def __new__(cls, exprc, exprp, exprn):
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
    def __init__(self, expr):
        self.expr = expr
        assert expr.arity() == 2

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
        # XXX
        print('{}{} # {}'.format(indent, e, type(e)))

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
        print('{}if ({}) {{ // DD'.format(indent, s.expr))
        sdisps(nest+1, s.stmtp)
        print('{}}} else {{'.format(indent))
        sdisps(nest+1, s.stmtn)
        print('{}}}'.format(indent))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
    elif isinstance(s, StructReturn):
        print('{}// R'.format(indent))
        print('{}{}'.format(indent, s.block))
        print('{}return'.format(indent))
    elif isinstance(s, StructSwitch):
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
                if is_e(de.stmtp):
                    if de.stmtp.exit != e.exit:
                        return struct
                    assert not de.joins
                    cond_de = ExprCond(de.expr, ExprThen(de.stmtp.expr, ExprConstBool(1)), ExprConstBool(0))
                    cond_e = ExprThen(e.expr, ExprConstBool(1))
                    stmtp = StructExprE(ExprVoid(), e.exit)
                    stmtn = de.stmtn
                else:
                    assert is_e(de.stmtn)
                    if de.stmtn.exit != e.exit:
                        return struct
                    assert not de.joins
                    cond_de = ExprCond(de.expr, ExprConstBool(1), ExprThen(de.stmtp.expr, ExprConstBool(0)))
                    cond_e = ExprThen(e.expr, ExprConstBool(0))
                    stmtp = de.stmtp
                    stmtn = StructExprE(ExprVoid(), e.exit)
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
        if '-' in block:
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
        res = StructExprDD(
            ExprLeafTwo(block, mah_block.cond),
            struct[mah_block.outp],
            struct[mah_block.outn],
            {
                x: struct[x]
                for x in rdom[block] - {mah_block.outp, mah_block.outn}
            },
        )
    res = simplify(res)
    if block in exits[block]:
        res = StructLoop(block, res)
    struct[block] = res

structify(entry)

if args.print_halfstruct or args.debug_halfstruct:
    print('-' * 20 + ' FINAL HALF-STRUCTURE ' + '-' * 20)
    sdisps(0, struct[entry])

# PHASE 4: JOIN CALC

block_hardafter = {}
block_after = {}
block_join = {}
loop_join = {}
weight = {}

def calc_weight(block):
    res = 1
    for x in rdom[block]:
        res += calc_weight(x)
    weight[block] = res
    return res

calc_weight(entry)


def join(block, mah_after, mah_hard):
    if block in exits[block]:
        # XXX loop
        pass
    block_after[block] = mah_after
    block_hardafter[block] = mah_hard
    local_candidates = []
    for x in rdom[block]:
        if not (set(exits[x]) & rdom[block]):
            li = 0
            for y in rdom[block]:
                if x in exits[y]:
                    li += 1
            local_candidates.append((li, weight[x], x))
    best_local = max(local_candidates) if local_candidates else None
    mah_join = None
    if mah_after is None or mah_after not in exits[block] or (not mah_hard and local_candidates and best_local[1] > 1):
        if local_candidates:
            mah_join = best_local[2]
    elif mah_hard:
        if exits[block][mah_after] != block:
            mah_join = exits[block][mah_after]
            while dom[mah_join] != block:
                mah_join = dom[mah_join]
    block_join[block] = mah_join
    for x in rdom[block]:
        if x == mah_join or mah_join is None:
            join(x, mah_after, mah_hard)
        else:
            join(x, mah_join, True)


join(entry, None, False)

def jdisp(nest, block):
    indent = nest * '  '
    infos = [
        '{}{} {}'.format(indent, block, blocks[block])
    ]
    if exits[block]:
        infos.append('EXITS: {}'.format(', '.join('{}[{}]'.format(k, v) if v != block else k for k, v in exits[block].items())))
    if block_after[block] is not None:
        infos.append('AFTER: {} {}'.format(block_after[block], block_hardafter[block]))
    if block_join[block] is not None:
        infos.append('JOIN {}'.format(block_join[block]))
    if block in exits[block]:
        infos.append('LOOP HEAD')
    print('; '.join(infos))
    for c in rdom[block]:
        jdisp(nest+1, c)

if args.debug_final:
    print('-' * 60)
    jdisp(0, entry)

# PHASE 5: DISPLAY

if args.debug_domtree or args.print_domtree or args.debug_halfstruct or args.print_halfstruct or args.debug_final:
    print('-' * 30 + ' FINAL RESULT ' + '-' * 30)

def doit(nest, block):
    indent = nest * '  '
    print('{}{}'.format(indent, block))
    mah_block = blocks[block]
    after = block_after[block]
    if isinstance(mah_block, EndBlock):
        print('{}end'.format(indent))
        assert len(rdom[block]) == 0
        return None
    elif isinstance(mah_block, ReturnBlock):
        print('{}return'.format(indent))
        assert len(rdom[block]) == 0
        return None
    elif isinstance(mah_block, UncondBlock):
        if not rdom[block]:
            return mah_block.out
        assert rdom[block] == {mah_block.out}
        return doit(nest, mah_block.out)
    elif isinstance(blocks[block], CondBlock):
        join = block_join[block]
        if not rdom[block]:
            if mah_block.outp == after:
                neg = True
                join = after
            elif mah_block.outn == after:
                neg = False
                join = after
            else:
                neg = False
                after = None
        elif len(rdom[block]) == 1:
            if mah_block.outp == after:
                neg = True
                join = after
            elif mah_block.outn == after:
                neg = False
                join = after
            elif mah_block.outp not in rdom[block]:
                neg = False
                join = mah_block.outn
            else:
                neg = True
                join = mah_block.outp
        else:
            if join == mah_block.outp:
                neg = True
            else:
                neg = False
        if neg:
            print('{}if (!({})) {{'.format(indent, blocks[block].cond))
            first = mah_block.outn
            second = mah_block.outp
        else:
            print('{}if ({}) {{'.format(indent, blocks[block].cond))
            first = mah_block.outp
            second = mah_block.outn
        #print(block, first, second, join)
        if second == join:
            second = None
        elif join is None:
            join = second
            second = None
        #print(block, first, second, join)
        join_used = False
        orig_first = first
        if first in rdom[block]:
            first = doit(nest+1, first)
        if first is not None:
            if first == join:
                join_used = True
            else:
                print('{}  goto {}'.format(indent, first))
        orig_second = second
        if second is not None:
            print('{}}} else {{'.format(indent))
            if second in rdom[block]:
                second = doit(nest+1, second)
            if second is not None:
                if second == join:
                    join_used = True
                else:
                    print('{}  goto {}'.format(indent, second))
        print('{}}}'.format(indent))
        for x in rdom[block]:
            if x not in {orig_first, orig_second, join}:
                if join_used:
                    print('{}if (0) {{'.format(indent))
                    x = doit(nest+1, x)
                    if x is not None and x != join:
                        print('{}  goto {}'.format(indent, x))
                    print('{}}}'.format(indent))
                else:
                    if join is None:
                        join = doit(nest, x)
                    else:
                        x = doit(nest, x)
                        if x is not None:
                            if x == join:
                                join_used = True
                            else:
                                print('{}goto {}'.format(indent, x))
        orig_join = join
        if join is not None and join in rdom[block]:
            join = doit(nest, join)
        #print(block, after, orig_first, orig_second, orig_join, first, second, join)
        return join

final = doit(0, entry)
if final is not None:
    print('goto {}'.format(final))
