from .bb import BasicBlock
from veles.dis.sema import SemaVar, SemaConst, SemaSlct


class Finish:
    pass

class FinishHalt(Finish):
    pass

class FinishUnk(Finish):
    def __init__(self, nextpc):
        self.nextpc = nextpc

class FinishBlock(Finish):
    def __init__(self, block):
        self.block = block

class FinishCond(Finish):
    def __init__(self, cond, finp, finn):
        self.cond = cond
        self.finp = finp
        self.finn = finn


class TreeBlock:
    def __init__(self, tree, pos, parent, regstate_in):
        self.tree = tree
        self.pos = pos
        self.parent = parent
        self.regstate_in = regstate_in
        self.valid = False
        self.children = []
        self.front = []
        self.bb = None
        self.phi_regs = {}
        self.finish = None
        self.outs = []
        tree.blocks[pos] = self
        tree.invalid_roots.add(self)
        if self.parent is not None:
            self.parent.children.append(self)
            if self.tree.debug:
                print('CONNECT {} {}'.format(self.parent.pos, self.pos))

    def invalidate(self):
        if not self.valid:
            return
        if self.tree.debug:
            print('INVALIDATING {}'.format(self.pos))
        for insn in self.bb.insns:
            self.tree.insns[insn.start].remove(self)
            if not self.tree.insns[insn.start]:
                del self.tree.insns[insn.start]
        if self.tree.debug:
            for k, v in self.tree.insns.items():
                print('{}: {}'.format(k, ', '.join(str(x.pos) for x in v)))
        for child in self.children:
            if self.tree.debug:
                print('DISCO CHILD {}'.format(child.pos))
            child.invalidate()
            child.parent = None
            self.tree.invalid_roots.remove(child)
            self.tree.orphans.add(child)
        self.children = []
        self.valid = False
        self.tree.invalid_roots.add(self)

    def pc_to_finish(self, expr):
        if isinstance(expr, SemaConst):
            tgt = self.tree.do_edge(self, expr.val, self.bb.regstate)
            self.outs.append(tgt)
            return FinishBlock(tgt)
        elif isinstance(expr, SemaSlct):
            tgtp = self.pc_to_finish(expr.va)
            tgtn = self.pc_to_finish(expr.vb)
            return FinishCond(expr.cond, tgtp, tgtn)
        else:
            return FinishUnk(expr)

    def process(self):
        if self.tree.debug:
            print('GO {} {} {}, {}'.format(self.pos, self.tree.stops, set(self.tree.insns), set(self.tree.blocks)))
        self.bb = BasicBlock(
            self.regstate_in,
            self.tree.data,
            self.tree.data_base,
            self.tree.base,
            self.tree.isa,
            self.pos,
            self.tree.stops,
        )
        for insn in self.bb.insns:
            if self.tree.debug:
                print('INSN {}'.format(insn.start))
            if insn.start not in self.tree.insns:
                self.tree.insns[insn.start] = set()
            else:
                for inv in self.tree.insns[insn.start]:
                    if self.tree.debug:
                        print('COLLIDE {} {}'.format(self.pos, inv.pos))
                    self.tree.invalidate_queue.append(inv)
            self.tree.insns[insn.start].add(self)
        if self.tree.debug:
            print('VALIDATED {}'.format(self.pos))
            for k, v in self.tree.insns.items():
                print('{}: {}'.format(k, ', '.join(str(x.pos) for x in v)))
        self.outs = []
        if self.bb.nextpc is None:
            self.finish = FinishHalt()
        else:
            self.finish = self.pc_to_finish(self.bb.nextpc)
        self.valid = True

    def root_path(self):
        if self.parent is None:
            res = []
        else:
            res = self.parent.root_path()
        return res + [self]

    def recalc_front(self):
        new_front = [x for x in self.outs if x not in self.children]
        for child in self.children:
            for block in child.front:
                if block in self.children or block in new_front:
                    continue
                new_front.append(block)
        self.front = new_front

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
                scc = TreeScc(self, nodes)
                for node in nodes:
                    node.scc = scc
                self.child_sccs.append(scc)
            if self.tree.debug:
                print('SCC EXIT {:x}'.format(block.pos))
            return lowlink

        for child in self.children:
            if child not in index:
                make_scc(child)
        self.child_sccs.reverse()

    def find_loops(self, loop):
        while loop is not None and loop.root not in self.front:
            loop.front.append(self)
            loop = loop.parent
        if self in self.front:
            loop = TreeLoop(self, loop)
        self.loop = loop
        if loop is not None:
            loop.nodes.append(self)
        for child in self.children:
            child.find_loops(loop)


class TreeScc:
    def __init__(self, parent, nodes):
        self.parent = parent
        self.nodes = nodes
        self.front = []
        for node in nodes:
            for dst in node.front:
                if dst not in self.front:
                    self.front.append(dst)


class TreeLoop:
    def __init__(self, root, parent):
        self.root = root
        self.parent = parent
        self.nodes = []
        self.subloops = []
        self.front = []
        if parent is not None:
            parent.subloops.append(self)


class DecoTree:
    def __init__(self, data, data_base, base, isa, entry_pos, regstate, debug=False):
        self.data = data
        self.data_base = data_base
        self.base = base
        self.isa = isa
        self.entry_pos = entry_pos
        self.entry_regstate = regstate
        self.blocks = {}
        self.insns = {}
        self.invalid_roots = set()
        self.orphans = set()
        self.stops = set()
        self.debug = debug
        self.root = TreeBlock(self, self.entry_pos, None, regstate)
        while self.invalid_roots:
            this_round = self.invalid_roots
            self.invalid_roots = set()
            self.edge_queue = []
            self.invalidate_queue = []
            self.stops = set(self.blocks)
            for block in this_round:
                block.process()
            while self.edge_queue:
                src, dst, regstate = self.edge_queue.pop()
                if regstate is not None:
                    all_regs = set(regstate) | set(dst.regstate_in)
                    for reg in all_regs:
                        if reg not in regstate or reg not in dst.regstate_in or regstate[reg] != dst.regstate_in[reg]:
                            if reg in regstate:
                                width = regstate[reg].width
                            else:
                                width = dst.regstate_in[reg].width
                            if reg not in dst.phi_regs:
                                phi = SemaVar(width, 'phi_{:x}_{}'.format(dst.pos, reg))
                                dst.phi_regs[reg] = phi
                                dst.regstate_in[reg] = phi
                                self.invalidate_queue.append(dst)
                src_path = set(src.root_path())
                if dst.parent is not None and dst.parent not in src_path:
                    while dst.parent not in src_path:
                        old_parent = dst.parent
                        dst.parent.children.remove(dst)
                        dst.parent = dst.parent.parent
                        dst.parent.children.append(dst)
                        if self.debug:
                            print('MOVE {} {} [was {}]'.format(dst.parent.pos, dst.pos, old_parent.pos))
                        old_parent.recalc_front()
                    for block in dst.front:
                        self.edge_queue.append((dst, block, None))
                block = src
                while block != dst.parent:
                    if dst not in block.front:
                        block.front.append(dst)
                    block = block.parent
            for block in self.invalidate_queue:
                block.invalidate()
        self.root.post_process_sort()
        self.root.find_loops(None)

    def do_edge(self, src, dstpos, regstate):
        if dstpos not in self.blocks:
            if dstpos in self.insns:
                for block in self.insns[dstpos]:
                    self.invalidate_queue.append(block)
            return TreeBlock(self, dstpos, src, regstate)
        else:
            dst = self.blocks[dstpos]
            if dst in self.orphans:
                if self.debug:
                    print('RECONNECT {} {}'.format(src.pos, dst.pos))
                    print('CHILDREN {}'.format([x.pos for x in dst.children]))
                dst.parent = src
                dst.regstate_in = regstate
                dst.regstate_in.update(dst.phi_regs)
                src.children.append(dst)
                self.invalid_roots.add(dst)
                self.orphans.remove(dst)
            else:
                self.edge_queue.append((src, dst, regstate))
            return dst
