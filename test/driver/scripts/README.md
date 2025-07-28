<!--
 Copyright 2025 Winford (Uncle Grumpy) <winford@object.stream>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->
# esptool.sh and partition binary files

The partition binary files in this directory are used by esptool.sh to simulate the real esptool.py
dumping the partition table from an AtomVM installed device. They were built using esp-idf and the
following partition.csv contents were used to generate the partition tables:

## partition.bin
This partition table starts with the standard Erlang only partition table, but adds several extra
partitions used for tests to flash to an alternate partition and test failures for invalid partition
types.

```csv
# Name,   Type, SubType, Offset,  Size, Flags
# Note: if you change the phy_init or app partition offset, make sure to change the offset in Kconfig.projbuild
nvs,      data, nvs,       0x9000,     0x6000,   \
phy_init, data, phy,       0xf000,     0x1000,    \
factory,  app,  factory,  0x10000,   0x1C0000,     > these are standard partitions
boot.avm, data, phy,     0x1D0000,    0x40000,    /
main.avm, data, phy,     0x210000,   0x100000,   /
app1.avm, data, 0xAA,    0x310000,   0x070000,   \
bad1,     data, fat,     0x380000,   0x010000,    > extra test partitions
bad2,     app,  test,    0x390000,   0x010000    /
```

## partition_elixir.bin
This partition table is just the standard Elixir supported build partition table.

```csv
# Name,   Type, SubType, Offset,  Size, Flags
# Note: if you change the phy_init or app partition offset, make sure to change the offset in Kconfig.projbuild
nvs,      data, nvs,       0x9000,     0x6000,
phy_init, data, phy,       0xf000,     0x1000,
factory,  app,  factory,  0x10000,   0x1C0000,
boot.avm, data, 0xAB,    0x1D0000,    0x80000,
main.avm, data, 0xAA,    0x250000,   0x100000
```
