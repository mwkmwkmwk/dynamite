class SpecialMode:
    MEMORY = 1
    VOLATILE = 2
    READ_VOLATILE = 4


class Special:
    def __init__(self, name, mode, outs, ins):
        self.name = name
        self.mode = mode
        self.outs = outs
        self.ins = ins


class SpecialHalt:
    def __init__(self, name, ins):
        self.name = name
        self.ins = ins


class Syscall:
    def __init__(self, name, ins):
        self.name = name
        self.ins = ins
