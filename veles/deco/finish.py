class Finish:
    pass


class FinishGoto(Finish):
    def __init__(self, block, dst, extra):
        self.block = block
        self.dst = dst
        self.extra = extra


class FinishJump(Finish):
    def __init__(self, block, addr, extra):
        self.block = block
        self.addr = addr
        self.extra = extra


class FinishCond(Finish):
    def __init__(self, block, cond, finp, finn):
        self.block = block
        self.cond = cond
        self.finp = finp
        self.finn = finn


class FinishCall(Finish):
    def __init__(self, block, tree, extra):
        self.block = block
        self.tree = tree
        self.extra = extra
        # XXX returns


# XXX FinishReturn
