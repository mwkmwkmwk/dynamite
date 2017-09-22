# What you're gonna do
# Time is running out on you
# Anyway you choose
# Anyway you're gonna lose

from collections import Counter

from .ir import (
    const_cuts,
    cut_ranges,
    IrVar, IrConst,
    IrParam, IrPhi, IrCallRes,
    IrExpr,
    IrAdd, IrSub, IrMul,
    IrAnd, IrOr, IrXor,
    IrExtr, IrSext, IrConcat,
    IrSlct,
    IrOpRes,
    IrGoto, IrCond, IrCall, IrJump, IrHalt, IrReturn
)

FOLDING = object()


class DecoBlock:
    def __init__(self):
        self.tree = None
        self.parent = None
        self.ins = []
        self.children = []
        self.valid = False
        self.front = []
        self.name = None
        self.phis = {}
        self.debug = False

    def init_entry(self, tree):
        assert self.tree is None
        self.tree = tree
        self.forest = tree.forest
        self.debug = self.tree.debug
        self.parent = None
        self.sub_init_entry()
        self.forest.enqueue_block(self)
        if self.debug:
            print('init entry {}'.format(self))

    def init_input(self, finish):
        assert not self.valid
        self.sub_init_input(finish)
        self.forest.enqueue_block(self)
        if self.debug:
            print('init input {} {}'.format(self, self.parent))

    def move_under(self, parent):
        if self.parent is parent:
            return
        if self.parent is not None:
            self.parent.children.remove(self)
        self.tree = parent.tree
        self.forest = self.tree.forest
        self.debug = self.tree.debug
        if self.debug:
            print('move under {} {} -> {}'.format(self, self.parent, parent))
        self.parent = parent
        self.parent.children.append(self)

    def root_path(self):
        if self.parent is None:
            res = []
        else:
            res = self.parent.root_path()
        return res + [self]

    def detach(self):
        if self.tree is None:
            return
        if self.debug:
            print('detach {}'.format(self))
        self.invalidate()
        self.tree = None
        self.parent.children.remove(self)
        self.parent = None
        for finish in self.ins[:]:
            finish.block.invalidate()

    def invalidate(self):
        if not self.valid:
            return
        self.front = []
        if self.parent is not None:
            self.parent.recalc_front()
        if self.debug:
            print('invalidate {}'.format(self))
        for finish in self.outs():
            finish.dst.ins.remove(finish)
        self.valid = False
        self.sub_invalidate()
        if isinstance(self.finish, IrCall):
            self.finish.tree.del_caller(self)
        if isinstance(self.finish, IrReturn):
            self.finish.path.del_block(self)
        for child in self.children[:]:
            child.detach()
        if self.tree is not None:
            self.forest.enqueue_block(self)

    def outs(self):
        assert self.valid
        if self.finish is None:
            return []
        if isinstance(self.finish, IrGoto):
            return [self.finish]
        elif isinstance(self.finish, IrCond):
            return [self.finish.finp, self.finish.finn]
        elif isinstance(self.finish, IrCall):
            return [ret.finish for ret in self.finish.returns.values()]
        return []

    def recalc_front(self):
        if not self.valid:
            return
        new_front = [x.dst for x in self.outs() if x.dst not in self.children]
        for child in self.children:
            for block in child.front:
                if block in self.children or block in new_front:
                    continue
                new_front.append(block)
        self.front = new_front
        if self.debug:
            print('recalc front {}: {}'.format(self, ', '.join(str(x) for x in self.front)))

    def process(self):
        self.valid = True
        self.finish = None
        self.ops = []
        self.exprs = []
        self.expr_ctr = 0
        self.expr_cache = {}
        if self.debug:
            print('process {}'.format(self))
        assert self.tree is not None
        self.sub_process()
        if self.valid:
            if self.debug:
                print('process {} done ok'.format(self))
            for finish in self.outs():
                self.forest.enqueue_edge(self, finish.dst, finish)
                finish.dst.ins.append(finish)

    def make_expr(self, cls, *args):
        if self.parent is not None:
            for arg in args:
                if isinstance(arg, IrVar) and arg.block == self:
                    break
            else:
                return self.parent.make_expr(cls, *args)
        full_args = cls, *args
        if full_args in self.expr_cache:
            res = self.expr_cache[full_args]
            assert res is not FOLDING
            return res
        self.expr_cache[full_args] = FOLDING
        res = cls.fold(self, *args)
        if res is None:
            res = cls(self, 'expr_{}_{:x}_{}'.format(cls.etype.lower(), self.pos, self.expr_ctr), *args)
            self.expr_ctr += 1
        self.expr_cache[full_args] = res
        return res

    def sub_init_entry(self):
        raise NotImplementedError

    def sub_init_input(self, finish):
        raise NotImplementedError

    def add_input(self, finish):
        raise NotImplementedError

    def sub_process(self):
        raise NotImplementedError

    def sub_invalidate(self):
        pass

    def get_default_name(self):
        return 'block_id_{:x}'.format(id(self))

    def get_name(self):
        if self.name is not None:
            return self.name
        return self.get_default_name()

    def get_func_name(self):
        return 'func_id_{:x}'.format(id(self))

    def post_process_sort(self):
        for child in self.children:
            child.post_process_sort()
        self.child_sccs = []
        index = {}
        stack = []
        ctr = 0
        if self.tree.debug:
            print('SCC BEGIN {:x}'.format(self.pos))

        def make_scc(block):
            nonlocal ctr
            if self.tree.debug:
                print('SCC ENTER {:x} {} [{}]'.format(block.pos, ctr, ', '.join(hex(x.pos) for x in stack)))
            index[block] = lowlink = ctr
            ctr += 1
            stack.append(block)
            for dst in block.front:
                if dst in self.children:
                    if dst not in index:
                        lowlink = min(lowlink, make_scc(dst))
                    elif dst in stack:
                        lowlink = min(lowlink, index[dst])
            if lowlink == index[block]:
                idx = stack.index(block)
                nodes = stack[idx:]
                del stack[idx:]
                if self.tree.debug:
                    print('SCC TRIGGER {} [{}]'.format(', '.join(hex(x.pos) for x in nodes), ', '.join(hex(x.pos) for x in stack)))
                scc = DecoTreeScc(self, nodes)
                for node in nodes:
                    node.scc = scc
                self.child_sccs.append(scc)
            if self.tree.debug:
                print('SCC EXIT {:x}'.format(block.pos))
            return lowlink

        for child in reversed(self.children):
            if child not in index:
                make_scc(child)
        self.child_sccs.reverse()

    def find_loops(self):
        for scc in reversed(self.child_sccs):
            scc_loop = self.loop
            front = set(scc.front)
            while scc_loop is not None and not (front & set(scc_loop.nodes)):
                scc_loop.inner_front += scc.nodes
                for node in scc.nodes:
                    if node not in scc_loop.front:
                        scc_loop.front.append(node)
                scc_loop = scc_loop.parent
            if scc_loop is not None:
                scc_loop.nodes += scc.nodes
            if len(scc.nodes) > 1:
                scc_loop = DecoLoop(scc.nodes, scc_loop)
            for node in scc.nodes:
                cur_loop = scc_loop
                if node in node.front:
                    cur_loop = DecoLoop([node], scc_loop)
                node.loop = cur_loop
            for node in scc.nodes:
                node.find_loops()
        for fin in self.outs():
            if fin.dst not in self.children:
                cur_loop = self.loop
                while cur_loop is not None and fin.dst not in cur_loop.nodes:
                    if fin.dst not in cur_loop.front:
                        cur_loop.front.append(fin.dst)
                    cur_loop = cur_loop.parent

    def compute_clean_phis(self):
        if isinstance(self.finish, IrCall):
            self.finish.arg_vals = {}
            for arg in self.finish.tree.root.args:
                mask = self.forest.live_masks.get(arg, 0)
                if mask == 0:
                    continue
                val = self.get_passed_arg(arg)
                if val is not None:
                    self.finish.arg_vals[arg] = self.make_expr(IrAnd, val, IrConst(val.width, mask))
        elif isinstance(self.finish, IrReturn):
            self.finish.res_vals = {}
            for loc in self.finish.path.res_locs():
                mask = self.forest.live_rets.get((self.finish.path, loc), 0)
                if mask == 0:
                    continue
                val = self.get_passed_res(loc)
                self.finish.res_vals[loc] = self.make_expr(IrAnd, val, IrConst(val.width, mask))
        for fin in self.outs():
            fin.phi_vals = {}
            for phi in fin.dst.phis.values():
                mask = self.forest.live_masks.get(phi, 0)
                if mask == 0:
                    continue
                val = self.get_passed_phi(fin, phi.loc)
                fin.phi_vals[phi] = self.make_expr(IrAnd, val, IrConst(val.width, mask))
        for child in self.children:
            child.compute_clean_phis()

    def compute_counts(self):
        def bump_count(val):
            if isinstance(val, IrExpr):
                val.block.expr_counts[val] += 1

        weight = 1
        self.expr_counts = Counter()
        for child in self.children:
            child.compute_counts()
            weight += child.weight
        for op in self.ops:
            for val in op.ins:
                bump_count(val)
                weight += 1
            weight += 1
        for fin in self.outs():
            for val in fin.phi_vals.values():
                bump_count(val)
                weight += 1
            weight += 1
        if isinstance(self.finish, IrCond):
            bump_count(self.finish.cond)
            weight += 1
        if isinstance(self.finish, IrJump):
            bump_count(self.finish.addr)
            weight += 1
        if isinstance(self.finish, IrHalt):
            for val in self.finish.ins:
                bump_count(val)
                weight += 1
            weight += 1
        if isinstance(self.finish, IrCall):
            for val in self.finish.arg_vals.values():
                bump_count(val)
                weight += 1
            weight += 1
        if isinstance(self.finish, IrReturn):
            for val in self.finish.res_vals.values():
                bump_count(val)
                weight += 1
            weight += 1
        for expr in reversed(self.exprs):
            if self.expr_counts[expr]:
                for val, mask in expr.live_ins(-1):
                    bump_count(val)
                    weight += 1
                weight += 1
        self.weight = weight

    def __str__(self):
        return self.get_name()


class DecoReturn:
    def __init__(self, tree):
        self.tree = tree
        self.blocks = []

    def get_name(self):
        if self.name is not None:
            return self.name
        return self.get_default_name()

    def __str__(self):
        return self.get_name()

    def add_block(self, block):
        assert block not in self.blocks
        self.blocks.append(block)

    def del_block(self, block):
        assert block in self.blocks
        self.blocks.remove(block)


class DecoTree:
    def __init__(self, forest, root, debug=False):
        self.name = None
        self.forest = forest
        self.root = root
        self.debug = debug
        self.callers = []
        self.root.init_entry(self)
        for finish in root.ins[:]:
            finish.block.invalidate()

    def set_name(self, name):
        self.name = name

    def get_name(self):
        if self.name is not None:
            return self.name
        return self.root.get_func_name()

    def add_caller(self, caller):
        if self.debug:
            print('new caller {} -> {}'.format(caller, self))
        assert caller not in self.callers
        self.callers.append(caller)

    def del_caller(self, caller):
        if self.debug:
            print('gone caller {} -> {}'.format(caller, self))
        assert caller in self.callers
        self.callers.remove(caller)

    def invalidate_callers(self):
        if self.debug:
            print('invalidate callers {}'.format(self))
        for caller in self.callers[:]:
            if self.debug:
                print('invalidate caller {} -> {}'.format(caller, self))
            caller.invalidate()

    def __str__(self):
        return self.get_name()


class DecoForest:
    def __init__(self, debug=False):
        self.trees = []
        self.blocks = {}
        self.debug = debug
        self.block_queue = []
        self.edge_queue = []

    def mark_block(self, cls, *args):
        full_args = cls, *args
        if full_args in self.blocks:
            return self.blocks[full_args]
        else:
            res = cls(*args)
            self.blocks[full_args] = res
            return res

    def mark_function(self, block):
        if block.tree is not None and block.parent is None:
            return block.tree
        block.detach()
        tree = DecoTree(self, block, debug=self.debug)
        self.trees.append(tree)
        return tree

    def enqueue_block(self, block):
        self.block_queue.append(block)

    def enqueue_edge(self, src, dst, finish):
        self.edge_queue.append((src, dst, finish))

    def process(self):
        while self.edge_queue or self.block_queue:
            if self.edge_queue:
                src, dst, finish = self.edge_queue.pop()
                if self.debug:
                    print('edge {} {} {}'.format(src, dst, finish))
                if not src.valid:
                    continue
                if dst.tree is None:
                    if finish is not None:
                        dst.move_under(src)
                        dst.init_input(finish)
                elif src.tree == dst.tree and dst != src.tree.root:
                    src_path = set(src.root_path())
                    if dst.parent not in src_path:
                        while dst.parent not in src_path:
                            old_parent = dst.parent
                            dst.move_under(old_parent.parent)
                            old_parent.recalc_front()
                        for block in dst.front:
                            self.edge_queue.append((dst, block, None))
                    if finish is not None:
                        dst.add_input(finish)
                    cur = src
                    if dst.valid:
                        while cur != dst.parent:
                            if dst not in cur.front:
                                cur.front.append(dst)
                            cur = cur.parent
                else:
                    src.invalidate()
            else:
                block = self.block_queue.pop()
                if block.valid:
                    continue
                if block.tree is None:
                    continue
                block.process()

    def compute_live_masks(self):
        self.live_masks = {}
        self.live_rets = {}
        live_queue = []
        def mark_roots(block):
            if isinstance(block.finish, IrJump):
                live_queue.append((block.finish.addr, -1))
            if isinstance(block.finish, IrCond):
                live_queue.append((block.finish.cond, -1))
            if isinstance(block.finish, IrHalt):
                for val in block.finish.ins:
                    live_queue.append((val, -1))
            for op in block.ops:
                for val in op.ins:
                    live_queue.append((val, -1))
            for child in block.children:
                mark_roots(child)
        for tree in self.trees:
            mark_roots(tree.root)
        while live_queue:
            val, mask = live_queue.pop()
            if isinstance(val, IrConst):
                continue
            mask &= (1 << val.width) - 1
            if mask == 0:
                continue
            cur_mask = self.live_masks.get(val, 0)
            mask |= cur_mask
            if mask != cur_mask:
                self.live_masks[val] = mask
                if isinstance(val, IrParam):
                    for caller in val.block.tree.callers:
                        cval = caller.get_passed_arg(val)
                        if cval is not None:
                            live_queue.append((cval, mask))
                elif isinstance(val, IrPhi):
                    for fin in val.block.ins:
                        cval = fin.block.get_passed_phi(fin, val.loc)
                        live_queue.append((cval, mask))
                elif isinstance(val, IrCallRes):
                    key = val.ret_path, val.loc
                    cur_mask = self.live_rets.get(key, 0)
                    new_mask = cur_mask | mask
                    if cur_mask != new_mask:
                        self.live_rets[key] = new_mask
                        for block in val.ret_path.blocks:
                            cval = block.get_passed_res(val.loc)
                            live_queue.append((cval, mask))
                elif isinstance(val, IrExpr):
                    for cval, cmask in val.live_ins(mask):
                        live_queue.append((cval, cmask))
                elif isinstance(val, IrOpRes):
                    pass
                else:
                    print(type(val))
                    raise NotImplementedError

    def post_process(self):
        for tree in self.trees:
            tree.root.post_process_sort()
            tree.root.loop = None
            tree.root.find_loops()
        self.compute_live_masks()
        for tree in self.trees:
            tree.root.compute_clean_phis()
            tree.root.compute_counts()


class DecoTreeScc:
    def __init__(self, parent, nodes):
        self.parent = parent
        self.nodes = nodes
        self.front = []
        for node in nodes:
            for dst in node.front:
                if dst not in self.front and dst not in self.nodes:
                    self.front.append(dst)


class DecoLoop:
    def __init__(self, roots, parent):
        self.tree = roots[0].tree
        self.roots = roots
        self.parent = parent
        self.nodes = roots[:]
        self.subloops = []
        self.front = []
        self.inner_front = []
        if parent is not None:
            parent.subloops.append(self)

    def __str__(self):
        return 'loop_{}'.format(', '.join(str(x) for x in self.roots))
