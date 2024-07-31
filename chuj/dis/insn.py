from .field import IsaSwitch, MatchError
from .st import IsaSTInsn
from .sema import SemaUnkOp, SemaList, SemaReadArg, SemaWriteArg


class BaseInsn:
    """
    The base class for instruction templates.  Converts instruction fields
    to/from an instruction syntax tree.
    """

    def parse(self, state):
        """
        Parses this argument from disassembly state, generating an instruction
        syntax tree.
        """
        raise NotImplementedError


class Insn(BaseInsn):
    """
    A single instruction template.  Once parsing gets here, the instruction
    is known, and it's just a matter of parsing its arguments.
    """

    def __init__(self, name, sema, *args, mods=[]):
        self.name = name
        if not isinstance(sema, SemaList):
            assert isinstance(sema, list)
            sema = SemaList(sema)
        self.sema = sema
        self.args = args
        self.mods = mods

    def parse(self, state):
        args = [arg.parse(state) for arg in self.args]
        mods = []
        for mod in self.mods:
            mods += mod.parse(state)
        def rebuilder_arg(cls, *args):
            if cls is SemaReadArg:
                dst, arg = args
                return self.args[arg].sema_read(state, dst)
            elif cls is SemaWriteArg:
                arg, src = args
                return self.args[arg].sema_write(state, src)
            else:
                return cls(*args)
        sema = self.sema.rebuild(rebuilder_arg)
        return IsaSTInsn(self.name, args, mods), sema


class InsnSwitch(BaseInsn, IsaSwitch):
    """
    Selects an instruction based on the value of a field - see IsaSwitch
    and IsaMatch for how the matching is done.
    """
    def parse(self, state):
        try:
            i = self.find(state)
        except MatchError as e:
            state.errors.append(e)
            return IsaSTInsn(None), [SemaUnkOp()]
        return i.parse(state)
