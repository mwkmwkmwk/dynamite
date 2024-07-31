from .field import MatchError, IsaSwitch


class BaseMod:
    """
    The base class for insn modifier templates.  Converts instruction fields
    to/from a list of modifiers.
    """

    def parse(self, dstate):
        """
        Parses this modifier from disassembly state, generating a list of
        modifiers (which are just strings).
        """
        raise NotImplementedError


class Mod(BaseMod):
    """
    An always-present modifier.
    """

    def __init__(self, name):
        self.name = name

    def parse(self, dstate):
        return [self.name]


class ModNone(BaseMod):
    """
    A null modifier (empty list of modifiers).
    """

    def parse(self, dstate):
        return []


class ModSwitch(IsaSwitch):
    """
    Selects a modifier based on the value of a field - see IsaSwitch
    and IsaMatch for how the matching is done.
    """

    def parse(self, dstate):
        try:
            m = self.find(dstate)
        except MatchError as e:
            dstate.errors.append(e)
            return []
        return m.parse(dstate)


# XXX: redundant with ModSwitch + Mod + ModNone - perhaps reimplement using
# these?
class ModIf:
    """
    A modifier that is present or not based on the value of a single-bit
    bitfield.
    """

    def __init__(self, field, name):
        assert field.width == 1
        self.field = field
        self.name = name

    def parse(self, dstate):
        v, m = self.field.get(dstate)
        if m != 1:
            dstate.errors.append(MatchError("unknown modifier state"))
        if v:
            return [self.name]
        else:
            return []
