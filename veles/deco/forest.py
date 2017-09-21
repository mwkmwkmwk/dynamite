# What you're gonna do
# Time is running out on you
# Anyway you choose
# Anyway you're gonna lose

from .ir import IrVar, IrGoto, IrCond

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
        self.debug = False

    def init_entry(self, tree):
        assert self.tree is None
        self.tree = tree
        self.debug = self.tree.debug
        self.parent = None
        self.sub_init_entry()
        self.tree.forest.enqueue_block(self)
        if self.debug:
            print('init entry {}'.format(self))

    def init_input(self, finish):
        assert not self.valid
        self.sub_init_input(finish)
        self.tree.forest.enqueue_block(self)
        if self.debug:
            print('init input {} {}'.format(self, self.parent))

    def move_under(self, parent):
        if self.parent is parent:
            return
        if self.parent is not None:
            self.parent.children.remove(self)
        self.tree = parent.tree
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
        if self.debug:
            print('invalidate {}'.format(self))
        for finish in self.outs():
            finish.dst.ins.remove(finish)
        self.valid = False
        self.sub_invalidate()
        for child in self.children[:]:
            child.detach()
        if self.tree is not None:
            self.tree.forest.enqueue_block(self)

    def outs(self):
        assert self.valid
        if self.finish is None:
            return []
        if isinstance(self.finish, IrGoto):
            return [self.finish]
        elif isinstance(self.finish, IrCond):
            return [self.finish.finp, self.finish.finn]
        return []

    def recalc_front(self):
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
        self.sub_process()
        if self.valid:
            if self.debug:
                print('process {} done ok'.format(self))
            for finish in self.outs():
                self.tree.forest.enqueue_edge(self, finish.dst, finish)
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

    def find_loops(self, loop):
        while loop is not None and loop.root not in self.front:
            loop.front.append(self)
            loop = loop.parent
        if self in self.front:
            loop = DecoLoop(self, loop)
        self.loop = loop
        if loop is not None:
            loop.nodes.append(self)
        for child in self.children:
            child.find_loops(loop)

    def __str__(self):
        return self.get_name()


class DecoTree:
    def __init__(self, forest, root, debug=False):
        self.name = None
        self.forest = forest
        self.root = root
        self.debug = debug
        self.root.init_entry(self)
        for finish in root.ins[:]:
            finish.block.invalidate()

    def set_name(self, name):
        self.name = name

    def get_name(self):
        if self.name is not None:
            return self.name
        return self.root.get_func_name()


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

    def mark_function(self, cls, *args):
        block = self.mark_block(cls, *args)
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
                    assert finish is not None
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

    def post_process(self):
        for tree in self.trees:
            tree.root.post_process_sort()
            tree.root.find_loops(None)


class DecoTreeScc:
    def __init__(self, parent, nodes):
        self.parent = parent
        self.nodes = nodes
        self.front = []
        for node in nodes:
            for dst in node.front:
                if dst not in self.front:
                    self.front.append(dst)


class DecoLoop:
    def __init__(self, root, parent):
        self.root = root
        self.parent = parent
        self.nodes = []
        self.subloops = []
        self.front = []
        if parent is not None:
            parent.subloops.append(self)
