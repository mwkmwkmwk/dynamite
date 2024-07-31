class MemSpace:
    """
    Represents a memory space.  Fields:

    - name: a string or None, used to print arguments referencing this space.
    - width: width of bytes in this space, in bits
    - addr_width: width of addresses in this space, in bits
    """

    def __init__(self, name, width, addr_width):
        self.name = name
        self.width = width
        self.addr_width = addr_width
