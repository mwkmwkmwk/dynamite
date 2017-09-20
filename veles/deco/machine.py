from .forest import DecoBlock
from .finish import FinishGoto, FinishJump, FinishCond, FinishCall

from .bb import BasicBlock
from ..dis.sema import SemaConst, SemaSlct, SemaVar


class MachineBaseBlock(DecoBlock):
    def __init__(self, segment):
        super().__init__()
        self.segment = segment
        self.phis = {}

    def sub_init_input(self, finish):
        self.regstate_in = dict(finish.extra)
        self.regstate_in.update(self.phis)

    def add_input(self, finish):
        all_regs = set(finish.extra) | set(self.regstate_in)
        for reg in all_regs:
            if reg not in finish.extra or reg not in self.regstate_in or finish.extra[reg] != self.regstate_in[reg]:
                if reg in finish.extra:
                    width = finish.extra[reg].width
                else:
                    width = self.regstate_in[reg].width
                if reg not in self.phis:
                    phi = SemaVar(width, 'phi_{:x}_{}'.format(self.pos, reg))
                    self.phis[reg] = phi
                    self.regstate_in[reg] = phi
                    self.invalidate()


class MachineBlock(MachineBaseBlock):
    def __init__(self, segment, pos):
        super().__init__(segment)
        self.pos = pos
        self.segment.add_block(self)

    def sub_init_entry(self):
        self.regstate_in = {}

    def sub_process(self):
        self.insn_starts = set()
        self.bb = BasicBlock(
            self.regstate_in,
            self.segment.data,
            self.segment.data_base,
            self.segment.base,
            self.segment.isa,
            self.pos,
            self.segment.blocks,
        )
        for insn in self.bb.insns:
            self.insn_starts.add(insn.start)
        self.segment.add_insns(self)
        if self.bb.nextpc is None:
            self.finish = None
        else:
            dst = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.bb.end, self.bb.nextpc)
            self.finish = FinishGoto(self, dst, self.bb.regstate)

    def sub_invalidate(self):
        self.segment.del_insns(self)

    def get_default_name(self):
        return 'block_{:x}'.format(self.pos)

    def get_func_name(self):
        return 'func_{:x}'.format(self.pos)


class MachineEndBlock(MachineBaseBlock):
    def __init__(self, segment, pos, target):
        super().__init__(segment)
        self.pos = pos
        self.target = target

    def sub_process(self):
        if isinstance(self.target, SemaConst):
            block = self.tree.forest.mark_block(MachineBlock, self.segment, self.target.val)
            if block.tree == self.tree or block.tree is None:
                self.finish = FinishGoto(self, block, self.regstate_in)
            else:
                self.finish = FinishCall(self, block.tree, self.regstate_in)
        elif isinstance(self.target, SemaSlct):
            tgtp = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.va)
            tgtn = self.tree.forest.mark_block(MachineEndBlock, self.segment, self.pos, self.target.vb)
            self.finish = FinishCond(self, self.target.cond,
                FinishGoto(self, tgtp, self.regstate_in),
                FinishGoto(self, tgtn, self.regstate_in),
            )
        else:
            self.finish = FinishJump(self, self.target, self.regstate_in)


class MachineSegment:
    def __init__(self, isa, data, data_base, base):
        self.isa = isa
        self.data = data
        self.data_base = data_base
        self.base = base
        self.blocks = {}
        self.insns = {}

    def add_block(self, block):
        self.blocks[block.pos] = block
        if block.pos in self.insns:
            for other in set(self.insns[block.pos]):
                other.invalidate()

    def add_insns(self, block):
        for pos in block.insn_starts:
            if pos not in self.insns:
                self.insns[pos] = set()
            self.insns[pos].add(block)

    def del_insns(self, block):
        for pos in block.insn_starts:
            if pos in self.insns:
                self.insns[pos].remove(block)
                if not self.insns[pos]:
                    del self.insns[pos]
