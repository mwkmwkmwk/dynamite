from .ir import (
    IrConst, IrExpr, IrOpRes,
    IrBin, IrConcat, IrExtr, IrSext,
    IrCF, IrOF, IrAddX,
    IrSlct,
    IrSpecial, IrLoad, IrStore, IrReadReg, IrWriteReg,
    IrGoto, IrJump, IrCond, IrHalt, IrReturn, IrCall
)
from veles.data.repack import Endian


class StmtBlock:
    def __init__(self, block):
        self.block = block

    def str(self, func, indent):
        if self.block in func.goto_targets:
            res = '{}:\n'.format(self.block)
        else:
            res = '{}// {}\n'.format(indent, self.block)
        printed_ops = set()
        ops = iter(self.block.ops)
        for expr in self.block.exprs:
            if self.block.expr_counts[expr] > 1:
                sub, things = func.str_expr(expr, 0, True)
                for thing in things:
                    if isinstance(thing, IrOpRes) and thing.block == self.block and thing.op not in printed_ops:
                        while thing.op not in printed_ops:
                            try:
                                op = next(ops)
                            except StopIteration:
                                print(thing.op, self.block, printed_ops, self.block.ops)
                                raise RuntimeError
                            res += self.str_op(func, indent, op)
                            printed_ops.add(op)
                res += '{}uint{}_t {} = {}\n'.format(indent, expr.width, expr, sub)
        for op in ops:
            res += self.str_op(func, indent, op)
        return res

    def str_op(self, func, indent, op):
        ins = [
            func.str_expr(val, 0)[0]
            for val in op.ins
        ]
        if isinstance(op, IrSpecial):
            main = '{}({})'.format(
                op.special.name,
                ', '.join(ins)
            )
        elif isinstance(op, IrStore):
            main = '*(uint{}{}_t {} *)({}) = {}'.format(
                op.ins[1].width,
                'le' if op.endian is Endian.LITTLE else 'be',
                op.mem.name,
                ins[0],
                ins[1],
            )
        elif isinstance(op, IrLoad):
            main = '*(uint{}{}_t {} *)({})'.format(
                op.outs[0].width,
                'le' if op.endian is Endian.LITTLE else 'be',
                op.mem.name,
                ins[0],
            )
        elif isinstance(op, IrWriteReg):
            main = '${} = {}'.format(
                op.reg.name,
                ins[0],
            )
        elif isinstance(op, IrReadReg):
            main = '${}'.format(
                op.reg.name,
            )
        else:
            print(type(op))
            raise NotImplementedError
        if any(self.block.tree.forest.live_masks.get(out, 0) for out in op.outs):
            return '{}{} = {};\n'.format(
                indent,
                ', '.join(
                    'uint{}_t {}'.format(out.width, out)
                    if self.block.tree.forest.live_masks.get(out, 0)
                    else '_'
                    for out in op.outs
                ),
                main,
            )
        else:
            return '{}{};\n'.format(indent, main)


class StmtGoto:
    def __init__(self, dst):
        self.dst = dst

    def str(self, func, indent):
        return '{}goto {};\n'.format(indent, self.dst)


class StmtContinue:
    def __init__(self, dst):
        self.dst = dst

    def str(self, func, indent):
        if self.dst is None:
            return '{}continue;\n'.format(indent)
        else:
            return '{}continue {};\n'.format(indent, self.dst)


class StmtBreak:
    def __init__(self, dst):
        self.dst = dst

    def str(self, func, indent):
        if self.dst is None:
            return '{}break;\n'.format(indent)
        else:
            return '{}break {};\n'.format(indent, self.dst)


class StmtPhi:
    def __init__(self, finish):
        self.finish = finish

    def str(self, func, indent):
        phis = []
        done = set()
        read = set()
        for phi in self.finish.phi_vals:
            val = self.finish.phi_vals[phi]
            s, t = func.str_expr(val, 0)
            phis.append((phi, s))
            for var in t:
                if var in done:
                    read.add(var)
            done.add(phi)
        res = ''
        for phi, val in phis:
            if phi in read:
                res += '{}next_{} = {};\n'.format(indent, phi, val)
            else:
                res += '{}{} = {};\n'.format(indent, phi, val)
        for phi, _ in phis:
            if phi in read:
                res += '{}{} = next_{};\n'.format(indent, phi, phi)
        return res


class StmtCallOne:
    def __init__(self, finish, results):
        self.finish = finish
        self.results = results

    def str(self, func, indent):
        main = '{}({})'.format(
            self.finish.tree,
            ', '.join(
                '{}={}'.format(arg, func.str_expr(val, 0)[0])
                for arg, val in self.finish.arg_vals.items()
            )
        )
        results = [res for res in self.results if func.tree.forest.live_rets.get(res.res, 0)]
        if len(results) == 0:
            return '{}{};\n'.format(indent, main)
        elif len(results) == 1:
            return '{}{} = {};\n'.format(indent, results[0], main)
        else:
            raise NotImplementedError


class StmtCallZero:
    def __init__(self, finish):
        self.finish = finish

    def str(self, func, indent):
        return '{}{}({});\n'.format(
            indent,
            self.finish.tree,
            ', '.join(
                '{}={}'.format(arg, func.str_expr(val, 0)[0])
                for arg, val in self.finish.arg_vals.items()
            )
        )


class StmtHalt:
    def __init__(self, finish):
        self.finish = finish

    def str(self, func, indent):
        ins = [
            func.str_expr(val, 0)[0]
            for val in self.finish.ins
        ]
        return '{}{}({});\n'.format(
            indent,
            self.finish.special.name,
            ', '.join(ins)
        )


class StmtReturn:
    def __init__(self, finish):
        self.finish = finish

    def str(self, func, indent):
        if len(self.finish.res_vals) == 0:
            return '{}return;\n'.format(
                indent,
            )
        elif len(self.finish.res_vals) == 1:
            val, = self.finish.res_vals.values()
            return '{}return {};\n'.format(
                indent,
                func.str_expr(val, 0)[0]
            )
        else:
            return '{}return ({});\n'.format(
                indent,
                ', '.join(
                    '{}={}'.format(res, func.str_expr(val, 0)[0])
                    for res, val in self.finish.res_vals.items()
                )
            )


class StmtJump:
    def __init__(self, finish):
        self.finish = finish

    def str(self, func, indent):
        return '{}goto *{};\n'.format(indent, func.str_expr(self.finish.addr, 0)[0])


class StmtIf:
    def __init__(self, cond, stmtsp, stmtsn):
        self.cond = cond
        self.stmtsp = stmtsp
        self.stmtsn = stmtsn

    def str(self, func, indent):
        if not self.stmtsn:
            return '{}if ({}) {{\n{}{}}}\n'.format(
                indent,
                func.str_expr(self.cond, 0)[0],
                ''.join(
                    stmt.str(func, indent + '    ')
                    for stmt in self.stmtsp
                ),
                indent
            )
        elif not self.stmtsp:
            return '{}if (!({})) {{\n{}{}}}\n'.format(
                indent,
                func.str_expr(self.cond, 0)[0],
                ''.join(
                    stmt.str(func, indent + '    ')
                    for stmt in self.stmtsn
                ),
                indent
            )
        else:
            return '{}if ({}) {{\n{}{}}} else {{\n{}{}}}\n'.format(
                indent,
                func.str_expr(self.cond, 0)[0],
                ''.join(
                    stmt.str(func, indent + '    ')
                    for stmt in self.stmtsp
                ),
                indent,
                ''.join(
                    stmt.str(func, indent + '    ')
                    for stmt in self.stmtsn
                ),
                indent,
            )


class StmtWhileTrue:
    def __init__(self, stmts):
        self.stmts = stmts

    def str(self, func, indent):
        return '{}while (1) {{\n{}{}}}\n'.format(
            indent,
            ''.join(
                stmt.str(func, indent + '    ')
                for stmt in self.stmts
            ),
            indent
        )


class StructFunc:
    def __init__(self, tree):
        self.tree = tree
        self.goto_targets = set()
        self.stmts = self.structuralize(tree.root, None, [], set(), {}, {})

    def emit_goto(self, block, next, loops, ends, breaks, continues):
        if block == next:
            return []
        if block in breaks:
            loop = breaks[block]
            if loop == loops[-1]:
                return [
                    StmtBreak(None),
                ]
            self.goto_targets.add(block)
            return [
                StmtBreak(block),
            ]
        if block in continues:
            loop = continues[block]
            if loop == loops[-1]:
                return [
                    StmtContinue(None),
                ]
            self.goto_targets.add(block)
            return [
                StmtContinue(block),
            ]
        self.goto_targets.add(block)
        return [
            StmtGoto(block),
        ]

    def emit_sub(self, finish, next, loops, ends, breaks, continues):
        res = self.emit_phi(finish)
        if finish.dst.scc in finish.block.simple_sccs and finish.dst not in ends:
            res += self.structuralize(finish.dst, next, loops, ends, breaks, continues)
        else:
            res += self.emit_goto(finish.dst, next, loops, ends, breaks, continues)
        return res

    def emit_phi(self, finish):
        if finish.phi_vals:
            return [
                StmtPhi(finish)
            ]
        else:
            return []

    def structuralize(self, block, next, loops, ends, breaks, continues):
        res = []
        if block.loop is not None and block.loop not in loops:
            end = None
            if next is not None:
                if next in block.loop.front:
                    end = next
                elif any(next in exit.front for exit in block.loop.inner_front):
                    exits = [exit for exit in block.loop.inner_front if next in exit.front]
                    if len(exits) == 1 and len(exits[0].scc.nodes) == 1:
                        end = exits[0]
                    else:
                        end = next
            if end is None:
                exit_exits = set()
                for exit in block.loop.inner_front:
                    exit_exits |= set(exit.front)
                for exit in block.loop.inner_front:
                    if exit in exit_exits and exit not in ends and len(exit.scc.nodes) == 1:
                        end = exit
                        break
            if end is None:
                best = 0
                for exit in block.loop.inner_front:
                    if exit.weight > best and exit not in ends and len(exit.scc.nodes) == 1:
                        end = exit
                        best = exit.weight
            new_breaks = dict(breaks)
            new_continues = dict(continues)
            if end is not None:
                new_breaks[end] = block.loop
            new_continues[block] = block.loop
            res.append(StmtWhileTrue(
                self.structuralize(
                    block,
                    block,
                    loops + [block.loop],
                    ends | {end} if end is not None else ends,
                    new_breaks,
                    new_continues,
                )
            ))
            if end is not None:
                if end in block.loop.inner_front and end not in ends:
                    res += self.structuralize(
                        end,
                        next,
                        loops,
                        ends,
                        breaks,
                        continues,
                    )
                else:
                    res += self.emit_goto(end, next, loops, ends, breaks, continues)
        else:
            res.append(StmtBlock(block))
            fin = block.simple_finish
            if isinstance(fin, IrHalt):
                res.append(StmtHalt(fin))
            elif isinstance(fin, IrJump):
                res.append(StmtJump(fin))
            elif isinstance(fin, IrReturn):
                res.append(StmtReturn(fin))
            elif isinstance(fin, IrGoto):
                res += self.emit_sub(fin, next, loops, ends, breaks, continues)
            elif isinstance(fin, IrCond):
                dstp = fin.finp.dst
                dstn = fin.finn.dst
                join = None
                emitp = True
                emitn = True
                if dstp == dstn:
                    emitp = emitn = False
                if dstp in ends:
                    emitp = False
                if len(dstp.scc.nodes) > 1:
                    emitp = False
                if dstp.scc not in block.simple_sccs:
                    emitp = False
                if dstn in ends:
                    emitn = False
                if len(dstn.scc.nodes) > 1:
                    emitn = False
                if dstn.scc not in block.simple_sccs:
                    emitn = False
                if emitp and emitn:
                    if dstn in dstp.front:
                        emitn = False
                    elif dstp in dstn.front:
                        emitp = False
                    elif len(block.simple_sccs) == 2 and next is None:
                        if dstp.weight > dstn.weight:
                            emitp = False
                        else:
                            emitn = False
                sccs = [
                    scc for scc in block.simple_sccs
                    if not (scc == dstp.scc and emitp) and not (scc == dstn.scc and emitn)
                    and not any(node in ends for node in scc.nodes)
                ]
                if sccs:
                    join = sccs[0].nodes[0]
                else:
                    join = next
                res.append(
                    StmtIf(
                        fin.cond,
                        self.emit_phi(fin.finp) + (
                            self.structuralize(dstp, join, loops, ends, breaks, continues)
                            if emitp
                            else self.emit_goto(dstp, join, loops, ends, breaks, continues)
                        ),
                        self.emit_phi(fin.finn) + (
                            self.structuralize(dstn, join, loops, ends, breaks, continues)
                            if emitn
                            else self.emit_goto(dstn, join, loops, ends, breaks, continues)
                        ),
                    ),
                )
                blocks = []
                for scc in sccs:
                    blocks += scc.nodes
                for sub, nsub in zip(blocks, blocks[1:] + [next]):
                    res += self.structuralize(sub, nsub, loops, ends, breaks, continues)
            elif isinstance(fin, IrCall):
                if len(fin.returns) > 1:
                    raise NotImplementedError
                elif len(fin.returns) == 1:
                    ret, = fin.returns.values()
                    res.append(StmtCallOne(fin, ret.results))
                    res += self.emit_sub(ret.finish, next, loops, ends, breaks, continues)
                else:
                    res.append(StmtCallZero(fin))
            else:
                raise NotImplementedError
        return res

    def str(self, indent):
        subind = indent + '    '
        args = ', '.join(
            'uint{}_t {} : {}'.format(arg.width, arg, arg.loc)
            for arg in self.tree.root.args
            if self.tree.forest.live_masks.get(arg, 0)
        )
        if len(self.tree.root.ret_paths) == 0:
            header = 'noreturn {}({})'.format(self.tree, args)
        elif len(self.tree.root.ret_paths) == 1:
            path = self.tree.root.ret_paths[0]
            results = [res for res in path.results if self.tree.forest.live_rets.get(res, 0)]
            if len(results) == 0:
                header = 'void {}({})'.format(self.tree, args)
            elif len(results) == 1:
                header = 'uint{}_t {}({}) -> {}'.format(results[0].width, self.tree, args, results[0].loc)
            else:
                raise NotImplementedError
        else:
            raise NotImplementedError
        return '{}{} {{\n{}{}}}\n'.format(
            indent, header,
            ''.join(stmt.str(self, subind) for stmt in self.stmts),
            indent
        )

    def str_expr(self, expr, lvl=0, force=False):
        if isinstance(expr, IrConst):
            return hex(expr.val), []
        elif isinstance(expr, IrExpr) and (expr.block.expr_counts[expr] <= 1 or force):
            if isinstance(expr, IrBin):
                if lvl > expr.lvl:
                    s, t = self.str_expr(expr, expr.lvl, force)
                    return '({})'.format(s), t
                sa, ta = self.str_expr(expr.va, expr.lvl)
                sb, tb = self.str_expr(expr.vb, expr.lvl + 1)
                return '{} {} {}'.format(
                    sa, expr.symbol, sb,
                ), ta + tb
            elif isinstance(expr, IrConcat):
                if lvl > 3:
                    s, t = self.str_expr(expr, 0, force)
                    return '({})'.format(s), t
                parts = []
                things = []
                for part in expr.cparts:
                    if part.sext is None:
                        if part.shift < 0:
                            p, t = self.str_expr(part.va, 6)
                            if part.va.width != expr.width:
                                sp = '(uint{}_t)({} >> {})'.format(expr.width, p, -part.shift)
                            else:
                                sp = '{} >> {}'.format(p, -part.shift)
                        elif part.shift == 0:
                            if part.va.width != expr.width:
                                p, t = self.str_expr(part.va, 9)
                                sp = '(uint{}_t){}'.format(expr.width, p)
                            else:
                                sp, t = self.str_expr(part.va, 4)
                        else:
                            if part.va.width != expr.width:
                                p, t = self.str_expr(part.va, 9)
                                sp = '(uint{}_t){} << {}'.format(expr.width, p, part.shift)
                            else:
                                p, t = self.str_expr(part.va, 6)
                                sp = '{} << {}'.format(p, part.shift)
                    else:
                        if part.shift < 0:
                            p, t = self.str_expr(part.va, 6)
                            sp = '(uint{}_t)((int{}_t){} >> {})'.format(expr.width, part.sext, p, -part.shift)
                        elif part.shift == 0:
                            p, t = self.str_expr(part.va, 9)
                            sp = '(uint{}_t)(int{}_t){}'.format(expr.width, part.sext, p)
                        else:
                            p, t = self.str_expr(part.va, 9)
                            sp = '(uint{}_t)(int{}_t){} << {}'.format(expr.width, part.sext, p, part.shift)
                    if part.mask is None:
                        parts.append(sp)
                    else:
                        parts.append('{} & {:#x}'.format(sp, part.mask))
                    things += t
                if expr.cpart_const:
                    parts.append(hex(expr.cpart_const))
                return ' | '.join(parts), things
            elif isinstance(expr, IrExtr):
                if expr.pos == 0:
                    s, t = self.str_expr(expr.va, 9)
                    return '(uint{}_t){}'.format(expr.width, s), t
                else:
                    s, t = self.str_expr(expr.va, 6)
                    return '(uint{}_t)({} >> {})'.format(expr.width, s, expr.pos), t
            elif isinstance(expr, IrSext):
                s, t = self.str_expr(expr.va, 9)
                return '(uint{}_t)(int{}_t){}'.format(expr.width, expr.va.width, s), t
            elif isinstance(expr, IrCF):
                sa, ta = self.str_expr(expr.va, 0)
                sb, tb = self.str_expr(expr.vb, 0)
                sc, tc = self.str_expr(expr.vc, 0)
                return 'CF({}, {}, {})'.format(sa, sb, sc), ta + tb + tc
            elif isinstance(expr, IrOF):
                sa, ta = self.str_expr(expr.va, 0)
                sb, tb = self.str_expr(expr.vb, 0)
                sc, tc = self.str_expr(expr.vc, 0)
                return 'OF({}, {}, {})'.format(sa, sb, sc), ta + tb + tc
            elif isinstance(expr, IrAddX):
                sa, ta = self.str_expr(expr.va, 0)
                sb, tb = self.str_expr(expr.vb, 0)
                sc, tc = self.str_expr(expr.vc, 0)
                return 'ADDX({}, {}, {})'.format(sa, sb, sc), ta + tb + tc
            elif isinstance(expr, IrSlct):
                sa, ta = self.str_expr(expr.va, 1)
                sb, tb = self.str_expr(expr.vb, 0)
                sc, tc = self.str_expr(expr.vc, 0)
                return '{} ? {} : {}'.format(sa, sb, sc), ta + tb + tc
            else:
                print(expr)
                raise NotImplementedError
        else:
            return str(expr), [expr]
