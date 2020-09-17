
;; WebAssembly portability assumes that execution environments offer
;; the following characteristics:

;; 8-bit bytes.

;; Addressable at a byte memory granularity.


;; Support unaligned memory accesses or reliable trapping that allows
;; software emulation thereof.

;; Two’s complement signed integers in 32 bits and optionally 64 bits.

;; IEEE 754-2008 32-bit and 64-bit floating point, except for a few
;; exceptions.

;; Little-endian byte ordering.

;; Memory regions which can be efficiently addressed with 32-bit
;; pointers or indices.

;; wasm64 additionally supports linear memory bigger than 4 GiB with
;; 64-bit pointers or indices.

;; Enforce secure isolation between WebAssembly modules and other
;; modules or processes executing on the same machine.

;; An execution environment which offers forward progress guarantees
;; to all threads of execution (even when executing in a non-parallel
;; manner).

;; Availability of lock-free atomic memory operators, when naturally
;; aligned, for 8- 16- and 32-bit accesses. At a minimum this must
;; include an atomic compare-and-exchange operator (or equivalent
;; load-linked/store-conditional).

;; wasm64 additionally requires lock-free atomic memory operators,
;; when naturally aligned, for 64-bit accesses.

;; there are 32, 32 bit integer registers
;; there are 64, 64 bit integer registers.
;; there are 32, 32-bit floating registers
;; there are 64, 64-bit floating registers.
