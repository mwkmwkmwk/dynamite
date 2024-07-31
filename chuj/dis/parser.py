from .field import IsaSwitch, MatchError


class ParseOOBError(Exception):
    """
    Insterted to dstate.errors to represent an OOB access to the code segment
    while decoding the instruction.
    """


class ParseState:
    def __init__(self, res, data, data_base, pos):
        self.res = res
        self.data = data
        self.data_base = data_base
        self.pos = pos


class ParseWord:
    """
    Fetches a single field from the code segment.  The width of the field must
    be a multiple of the code segment byte size.
    """

    def __init__(self, field, endian=None):
        self.field = field
        self.endian = endian

    def parse(self, pstate):
        if pstate.res.desync:
            return
        width = pstate.data.width
        nbytes = self.field.width // width
        assert self.field.width % width == 0
        if nbytes != 1:
            assert self.endian in ('little', 'big')
        mask = 0
        val = 0
        fail = False
        for x in range(nbytes):
            if self.endian == 'little':
                shift = x * width
            else:
                shift = (nbytes - 1 - x) * width
            rpos = pstate.pos + x - pstate.data_base
            if rpos >= 0 and rpos < len(pstate.data):
                val |= pstate.data[rpos] << shift
                mask |= ((1 << width) - 1) << shift
            else:
                fail = True
        self.field.set(pstate.res, val, mask)
        pstate.pos += nbytes
        if fail:
            err = ParseOOBError("reading {} failed".format(self.field))
            pstate.res.errors.append(err)


class ParseInsn:
    """
    Parses a single instruction based on the fields already read.
    """

    def __init__(self, action):
        self.action = action

    def parse(self, pstate):
        insn, sema = self.action.parse(pstate.res)
        pstate.res.insns.append(insn)
        pstate.res.sema += sema


class ParseSwitch(IsaSwitch):
    """
    Selects a parser based on the value of a field - see IsaSwitch
    and IsaMatch for how the matching is done.
    """

    def parse(self, pstate):
        try:
            m = self.find(pstate.res)
        except MatchError as e:
            pstate.res.errors.append(e)
            pstate.res.desync = True
            return
        if not isinstance(m, (list, tuple)):
            m = [m]
        for x in m:
            x.parse(pstate)


class ParseAnchor:
    def __init__(self, anchor):
        self.anchor = anchor

    def parse(self, pstate):
        pstate.res.anchors[self.anchor.name] = pstate.pos
