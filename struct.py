#!/usr/bin/env python3

import sys

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

with open(sys.argv[1]) as f:
    for l in f:
        l, _, _ = l.partition('#')
        p = l.split()
        if not p:
            continue
        b = p[0]
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

DEBUG_DOMTREE = False

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
    cur_from, cur_to = queue.pop()
    if cur_to not in dom:
        if DEBUG_DOMTREE:
            print('NEW', cur_from, cur_to)
        dom[cur_to] = cur_from
        exits[cur_to] = {}
        rdom[cur_to] = set()
        rdom[cur_from].add(cur_to)
        if cur_to not in blocks:
            raise ValueError("Unknown block {}".format(cur_to))
        for out in blocks[cur_to].outs():
            if DEBUG_DOMTREE:
                print('QUEUE', cur_to, out)
            queue.append((cur_to, out))
    else:
        if DEBUG_DOMTREE:
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
                    if DEBUG_DOMTREE:
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
            if DEBUG_DOMTREE:
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
                if DEBUG_DOMTREE:
                    print('REQUEUE', cur_to, x)
                queue.append((cur_to, x))
        orig_from = cur_from
        while cur_from != new_dom:
            exits[cur_from][cur_to] = orig_from
            cur_from = dom[cur_from]
    if DEBUG_DOMTREE:
        disp(0, entry)
        print('INFLIGHT', queue)
        print('-' * 60)

disp(0, entry)

# PHASE 3: EXPRESSIFY

DEBUG_STRUCT = False

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

# 2 pure
class StructExprEE:
    def __init__(self, expr, exitp, exitn):
        assert expr.arity() == 2
        self.expr = expr
        self.exitp = exitp
        self.exitn = exitn

    def exits(self):
        return {self.exitp, self.exitn}

# 1-hpure
class StructExprDE:
    def __init__(self, expr, stmt, exit):
        assert expr.arity() == 2
        self.expr = expr
        self.stmt = stmt
        self.exit = exit
        self._exits = {exit} | stmt.exits()

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

class StructExprDJ:
    def __init__(self, expr, stmtp, outn, joins):
        assert expr.arity() == 2
        self.expr = expr
        self.stmtp = stmtp
        self.outn = outn
        self.joins = joins
        self._exits = stmtp.exits()
        for stmt in self.joins.values():
            self._exits |= stmt.exits()
        self._exits -= set(self.joins)

    def exits(self):
        return self._exits

class StructExprJJ:
    def __init__(self, expr, outp, outn, joins):
        assert expr.arity() == 2
        self.expr = expr
        self.outp = outp
        self.outn = outn
        self.joins = joins
        self._exits = set()
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
        sdispe(nest, s.expr)
    elif isinstance(s, StructExprE):
        sdispe(nest, s.expr)
        print('{}goto {}'.format(indent, s.exit))
    elif isinstance(s, StructExprD):
        sdispe(nest, s.expr)
        sdisps(nest, s.stmt)
    elif isinstance(s, StructExprEE):
        print('{}if ({}) {{ # EE'.format(indent, s.expr))
        print('{}  goto {}'.format(indent, s.exitp))
        print('{}}} else {{'.format(indent))
        print('{}  goto {}'.format(indent, s.exitn))
        print('{}}}'.format(indent))
    elif isinstance(s, StructExprDE):
        print('{}if ({}) {{ # DE'.format(indent, s.expr))
        sdisps(nest+1, s.stmt)
        print('{}}}'.format(indent))
        print('{}goto {}'.format(indent, s.exit))
    elif isinstance(s, StructExprDD):
        print('{}if ({}) {{ # DD'.format(indent, s.expr))
        sdisps(nest+1, s.stmtp)
        print('{}}} else {{'.format(indent))
        sdisps(nest+1, s.stmtn)
        print('{}}}'.format(indent))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
    elif isinstance(s, StructExprDJ):
        print('{}if ({}) {{ # DJ'.format(indent, s.expr))
        sdisps(nest+1, s.stmtp)
        print('{}}}'.format(indent))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
    elif isinstance(s, StructExprJJ):
        print('{}if ({}) {{ # JJ'.format(indent, s.expr))
        print('{}  goto {}'.format(indent, s.outp))
        print('{}}} else {{'.format(indent))
        print('{}  goto {}'.format(indent, s.outn))
        print('{}}}'.format(indent))
        for k, v in s.joins.items():
            print('{}{}:'.format(indent, k))
            sdisps(nest+1, v)
    elif isinstance(s, StructReturn):
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

if DEBUG_STRUCT:
    print('-' * 80)

def simplify(struct):
    if DEBUG_STRUCT:
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
            elif isinstance(child, StructExprEE):
                struct = StructExprEE(
                    ExprThen(struct.expr, child.expr),
                    child.exitp,
                    child.exitn
                )
            elif isinstance(child, StructExprDE):
                struct = StructExprDE(
                    ExprThen(struct.expr, child.expr),
                    child.stmt,
                    child.exit
                )
            elif isinstance(child, StructExprDD):
                struct = StructExprDD(
                    ExprThen(struct.expr, child.expr),
                    child.stmtp,
                    child.stmtn,
                    child.joins,
                )
            elif isinstance(child, StructExprDJ):
                struct = StructExprDJ(
                    ExprThen(struct.expr, child.expr),
                    child.stmtp,
                    child.outn,
                    child.joins,
                )
            elif isinstance(child, StructExprJJ):
                struct = StructExprJJ(
                    ExprThen(struct.expr, child.expr),
                    child.outp,
                    child.outn,
                    child.joins,
                )
            else:
                return struct
        elif isinstance(struct, StructExprDE):
            child = struct.stmt
            if isinstance(child, StructExprZ):
                struct = StructExprE(
                    ExprCond(
                        struct.expr,
                        child.expr,
                        ExprVoid()
                    ),
                    struct.exit
                )
            elif isinstance(child, StructExprE):
                if child.exit == struct.exit:
                    struct = StructExprE(
                        ExprCond(
                            struct.expr,
                            child.expr,
                            ExprVoid()
                        ),
                        struct.exit
                    )
                else:
                    struct = StructExprEE(
                        ExprCond(
                            struct.expr,
                            ExprThen(child.expr, ExprConstBool(1)),
                            ExprConstBool(0)
                        ),
                        child.exit,
                        struct.exit
                    )
            elif isinstance(child, StructExprEE):
                if child.exitp == struct.exit:
                    struct = StructExprEE(
                        ExprCond(
                            struct.expr,
                            child.expr,
                            ExprConstBool(1)
                        ),
                        child.exitp,
                        child.exitn
                    )
                elif child.exitn == struct.exit:
                    struct = StructExprEE(
                        ExprCond(
                            struct.expr,
                            child.expr,
                            ExprConstBool(0)
                        ),
                        child.exitp,
                        child.exitn
                    )
                else:
                    return struct
            elif isinstance(child, StructExprD):
                struct = StructExprDE(
                    ExprCond(
                        struct.expr,
                        ExprThen(child.expr, ExprConstBool(1)),
                        ExprConstBool(0)
                    ),
                    child.stmt,
                    struct.exit
                )
            elif isinstance(child, StructExprDE):
                if child.exit == struct.exit:
                    struct = StructExprDE(
                        ExprCond(
                            struct.expr,
                            child.expr,
                            ExprConstBool(0)
                        ),
                        child.stmt,
                        child.exit
                    )
                else:
                    return struct
            else:
                return struct
        elif isinstance(struct, StructExprDD):
            if isinstance(struct.stmtp, StructExprD):
                struct = StructExprDD(
                    ExprCond(
                        struct.expr,
                        ExprThen(struct.stmtp.expr, ExprConstBool(1)),
                        ExprConstBool(0)
                    ),
                    struct.stmtp.stmt,
                    struct.stmtn,
                    struct.joins
                )
            elif isinstance(struct.stmtn, StructExprD):
                struct = StructExprDD(
                    ExprCond(
                        struct.expr,
                        ExprConstBool(1),
                        ExprThen(struct.stmtn.expr, ExprConstBool(0))
                    ),
                    struct.stmtp,
                    struct.stmtn.stmt,
                    struct.joins
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
            elif isinstance(struct.stmtn, StructExprE) and not struct.joins:
                struct = StructExprDE(
                    ExprCond(
                        struct.expr,
                        ExprConstBool(1),
                        ExprThen(struct.stmtn.expr, ExprConstBool(0)),
                    ),
                    struct.stmtp,
                    struct.stmtn.exit
                )
            elif isinstance(struct.stmtp, StructExprE) and not struct.joins:
                struct = StructExprDE(
                    ExprCond(
                        struct.expr,
                        ExprThen(struct.stmtp.expr, ExprConstBool(0)),
                        ExprConstBool(1)
                    ),
                    struct.stmtn,
                    struct.stmtp.exit
                )
            elif isinstance(struct.stmtp, StructExprE) and isinstance(struct.stmtn, StructExprE):
                if struct.stmtp.exit != struct.stmtn.exit:
                    return struct
                assert len(struct.joins) == 1
                assert struct.stmtp.exit in struct.joins
                struct = StructExprD(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        struct.stmtn.expr
                    ),
                    struct.joins[struct.stmtp.exit]
                )
            elif (isinstance(struct.stmtp, (StructExprE, StructExprEE)) and
                    isinstance(struct.stmtn, (StructExprE, StructExprEE))):
                if isinstance(struct.stmtp, StructExprEE):
                    condp = struct.stmtp.expr
                    exitp = struct.stmtp.exitp
                    exitn = struct.stmtp.exitn
                    if isinstance(struct.stmtn, StructExprE):
                        if struct.stmtn.exit == exitp:
                            condn = ExprThen(struct.stmtn.expr, ExprConstBool(1))
                        elif struct.stmtn.exit == exitn:
                            condn = ExprThen(struct.stmtn.expr, ExprConstBool(0))
                        else:
                            return struct
                    else:
                        assert isinstance(struct.stmtn, StructExprEE)
                        if struct.stmtn.exitp == exitp and struct.stmtn.exitn == exitn:
                            condn = struct.stmtn.expr
                        elif struct.stmtn.exitp == exitn and struct.stmtn.exitn == exitp:
                            condn = ExprNot(struct.stmtn.expr)
                        else:
                            return struct
                else:
                    assert isinstance(struct.stmtp, StructExprE)
                    assert isinstance(struct.stmtn, StructExprEE)
                    exitp = struct.stmtn.exitp
                    exitn = struct.stmtn.exitn
                    condn = struct.stmtn.expr
                    if struct.stmtp.exit == exitp:
                        condp = ExprThen(struct.stmtp.expr, ExprConstBool(1))
                    elif struct.stmtp.exit == exitn:
                        condp = ExprThen(struct.stmtp.expr, ExprConstBool(0))
                    else:
                        return struct
                cond = ExprCond(struct.expr, condp, condn)
                if not struct.joins:
                    struct = StructExprEE(cond, exitp, exitn)
                elif len(struct.joins) == 1:
                    assert exitp in struct.joins or exitn in struct.joins
                    if exitn in struct.joins:
                        exitp, exitn = exitn, exitp
                        cond = ExprNot(cond)
                    struct = StructExprDE(cond, struct.joins[exitp], exitn)
                else:
                    assert exitp in struct.joins and exitn in struct.joins
                    struct = StructExprJJ(cond, exitp, exitn, struct.joins)
            elif isinstance(struct.stmtp, StructExprDE) and isinstance(struct.stmtn, StructExprE):
                if struct.stmtp.exit != struct.stmtn.exit:
                    return struct
                if struct.stmtp.exit in struct.stmtp.stmt.exits():
                    return struct
                for x in struct.joins.values():
                    if struct.stmtp.exit in x.exits():
                        return struct
                struct = StructExprDD(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        ExprThen(struct.stmtn.expr, ExprConstBool(0))
                    ),
                    struct.stmtp.stmt,
                    struct.joins[struct.stmtp.exit],
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.stmtp.exit
                    }
                )
            elif isinstance(struct.stmtp, StructExprE) and isinstance(struct.stmtn, StructExprDE):
                if struct.stmtp.exit != struct.stmtn.exit:
                    return struct
                if struct.stmtn.exit in struct.stmtn.stmt.exits():
                    return struct
                for x in struct.joins.values():
                    if struct.stmtp.exit in x.exits():
                        return struct
                struct = StructExprDD(
                    ExprCond(
                        struct.expr,
                        ExprThen(struct.stmtp.expr, ExprConstBool(0)),
                        struct.stmtn.expr,
                    ),
                    struct.stmtn.stmt,
                    struct.joins[struct.stmtn.exit],
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.stmtp.exit
                    }
                )
            else:
                return struct
        elif isinstance(struct, StructExprDJ):
            if isinstance(struct.stmtp, StructExprE):
                assert struct.stmtp.exit == struct.outn
                assert len(struct.joins) == 1
                assert struct.outn in struct.joins
                struct = StructExprD(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        ExprVoid()
                    ),
                    struct.joins[struct.outn]
                )
            elif isinstance(struct.stmtp, StructExprDE):
                if struct.stmtp.exit != struct.outn:
                    return struct
                if struct.stmtp.exit in struct.stmtp.stmt.exits():
                    return struct
                assert struct.outn in struct.joins
                struct = StructExprDD(
                    ExprCond(
                        struct.expr,
                        struct.stmtp.expr,
                        ExprConstBool(0)
                    ),
                    struct.stmtp.stmt,
                    struct.joins[struct.outn],
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.outn
                    }
                )
            elif isinstance(struct.stmtp, StructExprEE):
                outp = struct.stmtp.exitp
                outn = struct.stmtp.exitn
                assert struct.outn in {outp, outn}
                if outp in struct.joins and outn in struct.joins:
                    struct = StructExprJJ(
                        ExprCond(
                            struct.expr,
                            struct.stmtp.expr,
                            ExprConstBool(1 if struct.outn == outp else 0)
                        ),
                        outp,
                        outn,
                        struct.joins
                    )
                else:
                    assert len(struct.joins) == 1
                    assert struct.outn in struct.joins
                    if outn == struct.outn:
                        struct = StructExprDE(
                            ExprCond(
                                struct.expr,
                                ExprNot(struct.stmtp.expr),
                                ExprConstBool(1)
                            ),
                            struct.joins[outn],
                            outp
                        )
                    else:
                        struct = StructExprDE(
                            ExprCond(
                                struct.expr,
                                struct.stmtp.expr,
                                ExprConstBool(0)
                            ),
                            struct.joins[outp],
                            outn
                        )
            else:
                return struct
        elif isinstance(struct, StructExprJJ):
            stmtp = struct.joins[struct.outp]
            stmtn = struct.joins[struct.outn]
            other_exits = {
                exit
                for k, v in struct.joins.items()
                if k not in {struct.outp, struct.outn}
                for exit in v.exits()
            }
            if {struct.outp, struct.outn} & other_exits:
                return struct
            p2n = struct.outn in stmtp.exits()
            n2p = struct.outp in stmtn.exits()
            if not p2n and not n2p:
                struct = StructExprDD(
                    struct.expr,
                    stmtp,
                    stmtn,
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k not in {struct.outp, struct.outn}
                    }
                )
            elif not n2p:
                struct = StructExprDJ(
                    struct.expr,
                    stmtp,
                    struct.outn,
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.outp
                    }
                )
            elif not p2n:
                struct = StructExprDJ(
                    ExprNot(struct.expr),
                    stmtn,
                    struct.outp,
                    {
                        k: v
                        for k, v in struct.joins.items()
                        if k != struct.outn
                    }
                )
            else:
                return struct
        else:
            return struct
        if DEBUG_STRUCT:
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
        res = StructExprJJ(
            ExprLeafTwo(block, mah_block.cond),
            mah_block.outp,
            mah_block.outn,
            {
                x: struct[x]
                for x in rdom[block]
            },
        )
    res = simplify(res)
    if block in exits[block]:
        res = StructLoop(block, res)
    struct[block] = res

structify(entry)

print('-' * 80)

sdisps(0, struct[entry])

print('-' * 80)

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

jdisp(0, entry)

# PHASE 5: DISPLAY

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
