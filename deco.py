#!/usr/bin/env python3

# Copyright 2017 CodiLime
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import argparse

parser = argparse.ArgumentParser(description='Decompile a list of functions.')
parser.add_argument('file', help='the input file')
parser.add_argument('-a', '--print-assembly', action='store_true', help='print the assembly insns')
parser.add_argument('-d', '--print-domtree', action='store_true', help='print dominator tree and complete register state')
parser.add_argument('-D', '--debug', action='store_true', help='print tree construction debug')
args = parser.parse_args()

from veles.data.bindata import BinData
from veles.dis.isa.falcon import FalconIsa
from veles.deco.forest import DecoForest
from veles.deco.machine import MachineSegment, MachineBlock, MachineBaseBlock, MachineReturn
from veles.deco.ir import IrGoto, IrCond, IrJump, IrCall, IrReturn, IrHalt
from veles.deco.struct import StructFunc

forest = DecoForest(debug=args.debug)

data = None
base = None
last_func = None

isa = FalconIsa()

with open(args.file) as f:
    for l in f:
        l, _, _ = l.partition('#')
        p = l.split()
        if not p:
            continue
        cmd = p[0]
        if data is None:
            if cmd != 'file':
                raise ValueError('Expecting a file command')
            if len(p) not in {2, 3, 5}:
                raise ValueError('File command needs 1, 2, or 4 arguments')
            if len(p) > 2:
                base = int(p[2], 16)
            else:
                base = 0
            with open(p[1], "rb") as df:
                if len(p) > 3:
                    df.seek(int(p[3], 16))
                    sz = int(p[4], 16)
                    data = df.read(sz)
                    if len(data) != sz:
                        raise ValueError('not enough data in the file')
                else:
                    data = df.read()
                data = BinData(8, data)
            segment = MachineSegment(isa, data, base, None)
        else:
            if cmd == 'func':
                if len(p) not in (2, 3):
                    raise ValueError('fun command needs 1 or 2 arguments')
                fun_start = int(p[1], 16)
                if len(p) >= 3:
                    fun_name = p[2]
                else:
                    fun_name = None
                fun = forest.mark_function(forest.mark_block(MachineBlock, segment, fun_start))
                fun.set_name(fun_name)
                last_func = fun
            elif cmd == 'nossa':
                if len(p) != 3:
                    raise ValueError('nossa command needs 2 arguments')
                off = int(p[2], 16)
                last_func.root.forbidden_stack_slots.add(off & 0xffffffff)
            elif cmd == 'arg':
                if len(p) != 3:
                    raise ValueError('arg command needs 2 arguments')
                reg = isa.regs[p[1]]
                last_func.root.make_arg(reg, p[2])
            else:
                raise ValueError('Unknown command "{}"'.format(cmd))

forest.process()
forest.post_process()


def print_finish(indent, finish):
    ind = '    ' * indent
    if finish is None:
        print('{}Halt'.format(ind))
    elif isinstance(finish, IrHalt):
        print('{}{}({})'.format(ind, finish.special.name, ', '.join(str(x) for x in finish.ins)))
        if isinstance(finish.extra, dict):
            for loc, val in finish.extra.items():
                print('{}    [{} = {}]'.format(ind, loc, val))
    elif isinstance(finish, IrGoto):
        print('{}goto {}'.format(ind, finish.dst.get_name()))
        for phi, val in finish.phi_vals.items():
            print('{}    {} = {}'.format(ind, phi, val))
        if isinstance(finish.extra, dict):
            for loc, val in finish.extra.items():
                print('{}    [{} = {}]'.format(ind, loc, val))
    elif isinstance(finish, IrCond):
        print('{}if ({}) {{'.format(ind, finish.cond))
        print_finish(indent + 1, finish.finp)
        print('{}}} else {{'.format(ind))
        print_finish(indent + 1, finish.finn)
        print('{}}}'.format(ind))
    elif isinstance(finish, IrJump):
        print('{}goto *{}'.format(ind, finish.addr))
        if isinstance(finish.extra, dict):
            for loc, val in finish.extra.items():
                print('{}    [{} = {}]'.format(ind, loc, val))
    elif isinstance(finish, IrCall):
        for arg, val in finish.arg_vals.items():
            print('{}    {} = {}'.format(ind, arg, val))
        if len(finish.returns) == 0:
            print('{}noreturn {}()'.format(ind, finish.tree.get_name()))
        elif len(finish.returns) == 1:
            print('{}{}()'.format(ind, finish.tree.get_name()))
            ret, = finish.returns.values()
            for res in ret.results:
                mask = forest.live_masks.get(res, 0)
                print('{}[{:x}] {} = {}'.format(ind, mask, res, res.res))
            print_finish(indent, ret.finish)
        else:
            print('{}{}() ->'.format(ind, finish.tree.get_name()))
            for path, ret in finish.returns.items():
                print('{}{}:'.format(ind, path))
                for res in ret.results:
                    mask = forest.live_masks.get(res, 0)
                    print('{}    [{:x}] {} = {}'.format(ind, mask, res, res.loc))
                print_finish(indent + 1, ret.finish)
    elif isinstance(finish, IrReturn):
        for loc, val in finish.res_vals.items():
            print('{}    {} = {}'.format(ind, loc, val))
        print('{}return {}()'.format(ind, finish.path))
        if isinstance(finish.extra, dict):
            for loc, val in finish.extra.items():
                print('{}    [{} = {}]'.format(ind, loc, val))
    else:
        print('{}???'.format(ind))


def print_bb(indent, block):
    ind = '    ' * indent
    if block.loop:
        if block.loop.roots == [block]:
            print('{}{}:'.format(ind[4:], block.loop))
            print('{}nodes: {}'.format(ind, ', '.join(x.get_name() for x in block.loop.nodes)))
            print('{}subloops: {}'.format(ind, ', '.join(str(x) for x in block.loop.subloops)))
            print('{}front: {}'.format(ind, ', '.join(x.get_name() for x in block.loop.front)))
        else:
            print('{}[in {}]'.format(ind[4:], block.loop))
    if block.front:
        print('{}{} [{}] [FRONT {}]:'.format(ind[4:], block.get_name(), block.weight, ', '.join(x.get_name() for x in block.front)))
    else:
        print('{}{} [{}]:'.format(ind[4:], block.get_name(), block.weight))
    if not block.valid:
        print('{}INVALID'.format(ind))
        print()
    else:
        if isinstance(block, MachineBlock):
            if args.print_assembly:
                for insn in block.raw_insns:
                    print('{}{:08x} {:18} {}'.format(ind, insn.start, ' '.join(format(x, '02x') for x in data[insn.start:insn.end]), (' '*28 + '\n').join(str(x) for x in insn.insns)))
                print()
        if isinstance(block, MachineBaseBlock):
            for loc, val in block.regstate_in.items():
                print('{}    [{} = {}]'.format(ind, loc, val))
        for loc, phi in block.phis.items():
            mask = forest.live_masks.get(phi, 0)
            print('{}[{:x}] {} = {}'.format(ind, mask, phi, loc))
        for op in block.ops:
            print('{}{}'.format(ind, op))
        for expr in block.exprs:
            mask = forest.live_masks.get(expr, 0)
            count = expr.block.expr_counts[expr]
            print('{}[{}] [{:x}] {}'.format(ind, count, mask, expr.display()))
        print_finish(indent, block.finish)
        print('{}simple [SCCS: {}]:'.format(ind, block.simple_sccs))
        print_finish(indent, block.simple_finish)
        print()
        for scc in block.child_sccs:
            if len(scc.nodes) == 1:
                print_bb(indent + 1, scc.nodes[0])
            else:
                if scc.front:
                    print('{}unnatural loop [FRONT {}]:'.format(ind, ', '.join('block_{:x}'.format(x.pos) for x in scc.front)))
                else:
                    print('{}unnatural loop:'.format(ind))
                for node in scc.nodes:
                    print_bb(indent + 2, node)


if args.print_domtree:
    for tree in forest.trees:
        print('Function {}:'.format(tree.get_name()))
        for path in tree.root.ret_paths:
            print('    Return path {}'.format(path))
            if isinstance(path, MachineReturn):
                print('        Results {}'.format(', '.join(str(x) for x in path.results)))
                print('        Stack offsets {}'.format(path.stack_offset))
        for arg in tree.root.args:
            mask = forest.live_masks.get(arg, 0)
            print('    [{:x}] {} = {}'.format(mask, arg, arg.loc))
        print_bb(1, tree.root)

for tree in forest.trees:
    s = StructFunc(tree)
    print('// Function at {:#x}.'.format(s.tree.root.pos))
    if s.tree.root.stack_slots:
        print('// Stack slots: {}.'.format(
            ', '.join(
                str(slot) for slot in s.tree.root.stack_slots.values()
            )
        ))
    print(s.str(''))
