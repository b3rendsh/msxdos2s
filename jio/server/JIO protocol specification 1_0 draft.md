# JIO protocol specification

This document describes the protocol used by the JIO server.  
Clients must adhere to this protocol to communicate correctly.  
No rights can be derived from this publication.  

**Protocol version:** 1.0 draft

## Overview

- Communication is unidirectional: the client sends commands, and the server responds.
- All messages start with a 3-byte signature.
- Commands can include an optional CRC.
- Responses depend on the command and are typically data blocks or acknowledgments.
- Data is transferred over a serial interface.
- Word and long word values are provided in little endian format.

## Packet structure

**Command packet:**

| Field     | Size        | Description                          |
|:----------|:------------|:-------------------------------------|
| Signature | 3 bytes     | Always 'JIO' (`0x4A 0x49 0x4F`)      |
| Flags     | 1 byte      | Bit 0: enable CRC checking (`0x01`)  |
| Command   | 1 byte      | Command identifier                   |
| Payload   | variable    | Command-specific data                |
| CRC       | 2 bytes     | Present only if CRC flag is set      |

**Response packet:**

| Field     | Size        | Description                       |
|:----------|:------------|:-----------------------------------
| Delay     | 7 bytes     | Processing delay 7x `0xFF`        |
| Sync      | 1 byte      | Synchronization byte `0xF0`       |
| Response  | variable    | Server response data              |

The number of delay bytes may vary. The server default is 7.  
The client should ignore all received `0xFF` delay bytes and proceed when it receives the `0xF0` sync byte.  

**CRC details:**

- CRC-16 used over all bytes starting after the signature up to the end of the payload.
- Not transmitted unless the CRC flag is set.
- Polynomial used: CRC-16-CCITT Xmodem

## Command list

#### 0x10 — COMMAND DRIVE READ

**Description:** Read block(s) of 512 bytes raw data

**Payload:** 
| Field     | Size        | Description                              | Example    |
|:----------|:------------|:-----------------------------------------|:-----------|
| Partition | 1 byte      | Partition number                         | `0x00`     |
| Sector    | 3 bytes     | Starting sector number                   | `0x010203` |
| Count     | 1 byte      | Number of 512-bytes sectors to read      | `0x10`     |
| Address   | 2 bytes     | Client destination memory address info   | `0xE0F0`   |

Note: the partition + sector is provided as a long word  
Example payload: `0x03` `0x02` `0x01` `0x00` `0x10` `0xF0` `0xE0`  

**Response:**  
Count * 512 bytes of raw data  

**RX CRC:**  
IF the RX CRC flag is set then an additional CRC packet is received:

| Field     | Size        | Description                       |
|:----------|:------------|:-----------------------------------
| Delay     | 7 bytes     | Processing delay 7x `0xFF`        |
| Sync      | 1 byte      | Synchronization byte `0xF0`       |
| CRC       | 2 bytes     | CRC value of the response data    |

  
#### 0x11 — COMMAND DRIVE WRITE

**Description:** Write block(s) of 512 bytes raw data

**Payload:**
| Field     | Size        | Description                            |
|:----------|:------------|:---------------------------------------|
| Partition | 1 byte      | Partition number                       |
| Sector    | 3 bytes     | Starting sector number                 |
| Count     | 1 byte      | Number of 512-bytes sectors to write   |
| Address   | 2 bytes     | Client source memory address info      |
| Data      | Count * 512 | Raw data to write                      |

Note: the partition + sector is provided as a long word  

**Response:**  
`0x11 0x11`: CRC mismatch (if TX CRC is enabled)  
`0x22 0x22`: Success  
`0x33 0x33`: Disk is write protected
  
  
#### 0x12 — COMMAND DRIVE INFO

**Description:** Request server metadata.  
**Payload:** none  
**Response:**
| Field     | Size        | Description                                            |
|:----------|:------------|:-------------------------------------------------------|
| Flags     | 1 byte      | Error detection flags                                  |
| Drives    | 1 byte      | Number of partitions (drives) on disk                  |
| Bootdrv   | 1 byte      | The partition number that is marked active (default 0) |
| Info      | 509 bytes   | Disk and flag information string (ends with 0x00)      |

**Error detection flags**  
| Bit | Flag           |
|----:|:---------------|
|   0 | RX CRC         |
|   1 | TX CRC         |
|   2 | TIMEOUT        |
|   3 | RETRY          |
|   4 | RESERVED       |
|   5 | RESERVED       |
|   6 | RESERVED       |
|   7 | RESERVED       |
  
  
#### 0x13 — COMMAND DRIVE DISK CHANGED

**Description:** Report if the disk image was changed since the command drive info or since the previous command drive disk changed. Applies to whole disk file not a partition or data content within the disk image.  
**Payload:** none  
**Response:**  
`0x44 0x44` : Changed  
`0x55 0x55` : Not changed
  
  
#### 0xNN — COMMAND DRIVE REPORT [NN]

**Description:** Report a disk i/o result to the server  
**Payload:** none  
**Response:** none

[NN]  
`0x00` : read/write ok (not used)  
`0x01` : drive write protected  
`0x03` : drive not ready / time-out  
`0x05` : CRC error (if RX CRC is enabled)  
`0x0B` : write fault

## Data transfer

### General setup

- The client and server are linked via a serial interface.
- The default transfer protocol is 115200 baud, no parity, 1 stop bit.
- The client always initiates a data transfer.
- The server is able to respond on each command immediately.
- Error detection and handling flags are set on the server only.
- Error handling logic is implemented on the client only.

### Command/response sequence

Example implementation of transmit command and receive response with error handling.

| Step    | Client | Server | Description                                                    |
|--------:|:------ |:-------|:---------------------------------------------------------------|
|       1 | X      |        | Transmit command                                               |
|       2 |        | X      | Receive command                                                |
|       3 |        | X      | If TX CRC flag set then validate CRC else goto 5               |
|       4 |        | X      | If CRC error then transmit response 'CRC mismatch': goto 6     |
|       5 |        | X      | Process command and transmit response                          |
|       6 | X      |        | Receive response until completed or timeout (if TIMEOUT flag)  |
|       7 | X      |        | If receive response timeout then goto 12                       |
|       8 | X      |        | If RX CRC flag set then validate CRC else goto 10              |
|       9 | X      |        | If CRC error then transmit report 'CRC error': goto 11         |
|      10 | X      |        | Process response and if error then transmit report             |
|      11 |        | X      | Optional: receive command report and display it in the log     |
|      12 | X      |        | If error and AUTORETRY flag set then goto 1                    |
|      13 | X      |        | Return to calling function with response result                |
