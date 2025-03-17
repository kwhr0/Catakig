// W65C02
// Copyright 2022-2025 © Yasuo Kuwahara
// MIT License

#include "W65C02.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>

enum {
	LC, LZ, LI, LD, LB, L1, LV, LN
};

enum {
	MC = 1 << LC, MZ = 1 << LZ, MI = 1 << LI, MD = 1 << LD,
	MB = 1 << LB, M1 = 1 << L1, MV = 1 << LV, MN = 1 << LN
};

enum {
	FD = 1, F0, F1, F8, FADD, FSUB, FLEFT, FRIGHT, FBIT
};

#define F(flag, type)	flag##type = F##type << (L##flag << 2)
enum {
	F(C, 0), F(C, 1), F(C, ADD), F(C, SUB), F(C, LEFT), F(C, RIGHT),
	F(Z, 8),
	F(D, 0), F(D, 1),
	F(V, D), F(V, 0), F(V, ADD), F(V, SUB),
	F(N, D), F(N, 8)
};

#define fclc()			fset<C0>()
#define fsec()			fset<C1>()
#define fcld()			fset<D0>()
#define fsed()			fset<D1>()
#define fclv()			fset<V0>()
#define fld(a)			fset<N8 | Z8>(a)
#define fadd(a, d, s)	fset<N8 | VADD | Z8 | CADD>(a, d, s)
#define fsub(a, d, s)	fset<N8 | VSUB | Z8 | CSUB>(a, d, s)
#define fcmp(a, d, s)	fset<N8 | Z8 | CSUB>(a, d, s)
#define fleft(a, d)		fset<N8 | Z8 | CLEFT>(a, d)
#define fright(a, d)	fset<N8 | Z8 | CRIGHT>(a, d)
#define fbit(a, d)		fset<ND | VD | Z8>(a, d)
#define fz(a)			fset<Z8>(a)

#define CY				(p & 1)

enum { W_WAI = 1, W_STP };

W65C02::W65C02() {
#if W65C02_TRACE
	memset(tracebuf, 0, sizeof(tracebuf));
	tracep = tracebuf;
#endif
}

void W65C02::Reset() {
	s = irq = waitflags = 0;
	p = MI | M1;
	// Catakig		pc = ld16(0xfffc);
}

int W65C02::Execute(int n) {
	auto lda = [&](u8 d) { fld(a = d); };
	auto ldx = [&](u8 d) { fld(x = d); };
	auto ldy = [&](u8 d) { fld(y = d); };
	auto sta = [&] { return a; };
	auto stx = [&] { return x; };
	auto sty = [&] { return y; };
	auto stz = [] { return 0; };
	auto adc = [&](u8 d) { a = fadd(a + d + CY, a, d); };
	auto sbc = [&](u8 d) { a = fsub(a - d - !CY, a, d); };
	auto cmp = [&](u8 d) { fcmp(a - d, a, d); };
	auto cpx = [&](u8 d) { fcmp(x - d, x, d); };
	auto cpy = [&](u8 d) { fcmp(y - d, y, d); };
	auto inc = [&](u8 d) { fld(++d); return d; };
	auto dec = [&](u8 d) { fld(--d); return d; };
	auto _and = [&](u8 d) { fld(a &= d); };
	auto ora = [&](u8 d) { fld(a |= d); };
	auto eor = [&](u8 d) { fld(a ^= d); };
	auto bit = [&](u8 d) { fbit(a & d, d); };
	auto biti = [&](u8 d) { fz(a & d); };
	auto tsb = [&](u8 d) { fz(a & d); return a | d; };
	auto trb = [&](u8 d) { fz(a & d); return ~a & d; };
	auto asl = [&](u8 d) { return fleft(d << 1, d); };
	auto rol = [&](u8 d) { return fleft(d << 1 | CY, d); };
	auto lsr = [&](u8 d) { return fright(d >> 1, d); };
	auto ror = [&](u8 d) { return fright(d >> 1 | CY << 7, d); };
	auto br = [&](u8 cond) {
		if (cond) {
			u16 last = pc;
			pc += (s8)imm8();
			clock += 2 + ((last & 0xff00) != (pc & 0xff00));
		}
		else {
			pc++;
			clock += 2;
		}
	};
	clock = 0;
	do {
		if (irq) {
			if (waitflags & W_WAI) {
				waitflags &= ~W_WAI;
				pc++;
			}
			if (irq & M_NMI) {
				irq &= ~M_NMI;
				st8(0x100 | s--, pc >> 8);
				st8(0x100 | s--, pc);
				st8(0x100 | s--, p & ~MB);
				p |= MI;
				pc = ld16(0xfffa);
				clock += 14; // p.142
			}
			else if (irq & M_IRQ && !(p & MI)) {
				irq &= ~M_IRQ;
				st8(0x100 | s--, pc >> 8);
				st8(0x100 | s--, pc);
				st8(0x100 | s--, p);
				p |= MI;
				pc = ld16(0xfffe);
				clock += 17; // p.139
			}
		}
#if W65C02_TRACE
		tracep->pc = pc;
		tracep->index = tracep->opn = 0;
#endif
		u8 t8;
		switch (imm8()) {
			case 0x00: ++pc; st8(0x100 | s--, pc >> 8); st8(0x100 | s--, pc); st8(0x100 | s--, p); pc = ld16(0xfffe); break; // brk
			case 0x01: rindx(ora); break;
			case 0x02: pc++; clock += 2; break;
			case 0x03: clock++; break;
			case 0x04: mzp(tsb); break;
			case 0x05: rzp(ora); break;
			case 0x06: mzp(asl); break;
			case 0x07: mzp([](u8 d) { return d & ~1; }); break;
			case 0x08: st8(0x100 | s--, p); clock += 3; break; // php
			case 0x09: imm(ora); break;
			case 0x0a: ma(asl); break;
			case 0x0b: clock++; break;
			case 0x0c: mabs(tsb); break;
			case 0x0d: rabs(ora); break;
			case 0x0e: mabs(asl); break;
			case 0x0f: rzp([&](u8 d) { br(!(d & 1)); }); break;
			case 0x10: br(!(p & MN)); break; // bpl
			case 0x11: rindy(ora); break;
			case 0x12: rzpp(ora); break;
			case 0x13: clock++; break;
			case 0x14: mzp(trb); break;
			case 0x15: rzpx(ora); break;
			case 0x16: mzpx(asl); break;
			case 0x17: mzp([](u8 d) { return d & ~2; }); break;
			case 0x18: fclc(); clock += 2; break; // clc
			case 0x19: rabsy(ora); break;
			case 0x1a: ma(inc); break;
			case 0x1b: clock++; break;
			case 0x1c: mabs(trb); break;
			case 0x1d: rabsx(ora); break;
			case 0x1e: mabsx(asl); break;
			case 0x1f: rzp([&](u8 d) { br(!(d & 2)); }); break;
			case 0x20: st8(0x100 | s--, (pc + 1) >> 8); st8(0x100 | s--, pc + 1); pc = imm16(); clock += 6; break; // jsr
			case 0x21: rindx(_and); break;
			case 0x22: pc++; clock += 2; break;
			case 0x23: clock++; break;
			case 0x24: rzp(bit); break;
			case 0x25: rzp(_and); break;
			case 0x26: mzp(rol); break;
			case 0x27: mzp([](u8 d) { return d & ~4; }); break;
			case 0x28: p = ld8(0x100 | ++s) | M1; clock += 4; break; // plp
			case 0x29: imm(_and); break;
			case 0x2a: ma(rol); break;
			case 0x2b: clock++; break;
			case 0x2c: rabs(bit); break;
			case 0x2d: rabs(_and); break;
			case 0x2e: mabs(rol); break;
			case 0x2f: rzp([&](u8 d) { br(!(d & 4)); }); break;
			case 0x30: br(p & MN); break; // bmi
			case 0x31: rindy(_and); break;
			case 0x32: rzpp(_and); break;
			case 0x33: clock++; break;
			case 0x34: rzpx(bit); break;
			case 0x35: rzpx(_and); break;
			case 0x36: mzpx(rol); break;
			case 0x37: mzp([](u8 d) { return d & ~8; }); break;
			case 0x38: fsec(); clock += 2; break; // sec
			case 0x39: rabsy(_and); break;
			case 0x3a: ma(dec); break;
			case 0x3b: clock++; break;
			case 0x3c: rabsx(bit); break;
			case 0x3d: rabsx(_and); break;
			case 0x3e: mabsx(rol); break;
			case 0x3f: rzp([&](u8 d) { br(!(d & 8)); }); break;
			case 0x40: p = ld8(0x100 | ++s) | M1; t8 = ld8(0x100 | ++s); pc = t8 | ld8(0x100 | ++s) << 8; clock += 6; break; // rti
			case 0x41: rindx(eor); break;
			case 0x42: pc++; clock += 2; break;
			case 0x43: clock++; break;
			case 0x44: pc++; clock += 3; break;
			case 0x45: rzp(eor); break;
			case 0x46: mzp(lsr); break;
			case 0x47: mzp([](u8 d) { return d & ~0x10; }); break;
			case 0x48: st8(0x100 | s--, a); clock += 3; break; // pha
			case 0x49: imm(eor); break;
			case 0x4a: ma(lsr); break;
			case 0x4b: clock++; break;
			case 0x4c: pc = imm16(); clock += 3; break; // jmp
			case 0x4d: rabs(eor); break;
			case 0x4e: mabs(lsr); break;
			case 0x4f: rzp([&](u8 d) { br(!(d & 0x10)); }); break;
			case 0x50: br(!(p & MV)); break; // bvc
			case 0x51: rindy(eor); break;
			case 0x52: rzpp(eor); break;
			case 0x53: clock++; break;
			case 0x54: pc++; clock += 4; break;
			case 0x55: rzpx(eor); break;
			case 0x56: mzpx(lsr); break;
			case 0x57: mzp([](u8 d) { return d & ~0x20; }); break;
			case 0x58: p &= ~MI; clock += 2; break; // cli
			case 0x59: rabsy(eor); break;
			case 0x5a: st8(0x100 | s--, y); clock += 3; break;
			case 0x5b: clock++; break;
			case 0x5c: pc += 2; clock += 8; break;
			case 0x5d: rabsx(eor); break;
			case 0x5e: mabsx(lsr); break;
			case 0x5f: rzp([&](u8 d) { br(!(d & 0x20)); }); break;
			case 0x60: t8 = ld8(0x100 | ++s); pc = (t8 | ld8(0x100 | ++s) << 8) + 1; clock += 6; break; // rts
			case 0x61: rindx(adc); break;
			case 0x62: pc++; clock += 2; break;
			case 0x63: clock++; break;
			case 0x64: wzp(stz); break;
			case 0x65: rzp(adc); break;
			case 0x66: mzp(ror); break;
			case 0x67: mzp([](u8 d) { return d & ~0x40; }); break;
			case 0x68: fld(a = ld8(0x100 | ++s)); clock += 4; break; // pla
			case 0x69: imm(adc); break;
			case 0x6a: ma(ror); break;
			case 0x6b: clock++; break;
			case 0x6c: pc = ld16(imm16()); clock += 5; break;
			case 0x6d: rabs(adc); break;
			case 0x6e: mabs(ror); break;
			case 0x6f: rzp([&](u8 d) { br(!(d & 0x40)); }); break;
			case 0x70: br(p & MV); break; // bvs
			case 0x71: rindy(adc); break;
			case 0x72: rzpp(adc); break;
			case 0x73: clock++; break;
			case 0x74: wzpx(stz); break;
			case 0x75: rzpx(adc); break;
			case 0x76: mzpx(ror); break;
			case 0x77: mzp([](u8 d) { return d & ~0x80; }); break;
			case 0x78: p |= MI; clock += 2; break; // sei
			case 0x79: rabsy(adc); break;
			case 0x7a: fld(y = ld8(0x100 | ++s)); clock += 4; break;
			case 0x7b: clock++; break;
			case 0x7c: pc = ld16(imm16() + x); clock += 6; break; // jmp (a,x)
			case 0x7d: rabsx(adc); break;
			case 0x7e: mabsx(ror); break;
			case 0x7f: rzp([&](u8 d) { br(!(d & 0x80)); }); break;
			case 0x80: br(1); break; // bra
			case 0x81: windx(sta); break;
			case 0x82: pc++; clock += 2; break;
			case 0x83: clock++; break;
			case 0x84: wzp(sty); break;
			case 0x85: wzp(sta); break;
			case 0x86: wzp(stx); break;
			case 0x87: mzp([](u8 d) { return d | 1; }); break;
			case 0x88: fld(--y); clock += 2; break; // dey
			case 0x89: imm(biti); break;
			case 0x8a: fld(a = x); clock += 2; break; // txa
			case 0x8b: clock++; break;
			case 0x8c: wabs(sty); break;
			case 0x8d: wabs(sta); break;
			case 0x8e: wabs(stx); break;
			case 0x8f: rzp([&](u8 d) { br(d & 1); }); break;
			case 0x90: br(!CY); break; // bcc
			case 0x91: windy(sta); break;
			case 0x92: wzpp(sta); break;
			case 0x93: clock++; break;
			case 0x94: wzpx(sty); break;
			case 0x95: wzpx(sta); break;
			case 0x96: wzpy(stx); break;
			case 0x97: mzp([](u8 d) { return d | 2; }); break;
			case 0x98: fld(a = y); clock += 2; break; // tya
			case 0x99: wabsy(sta); break;
			case 0x9a: fld(s = x); clock += 2; break; // txs
			case 0x9b: clock++; break;
			case 0x9c: wabs([] { return 0; }); break;
			case 0x9d: wabsx(sta); break;
			case 0x9e: wabsx(stz); break;
			case 0x9f: rzp([&](u8 d) { br(d & 2); }); break;
			case 0xa0: imm(ldy); break;
			case 0xa1: rindx(lda); break;
			case 0xa2: imm(ldx); break;
			case 0xa3: clock++; break;
			case 0xa4: rzp(ldy); break;
			case 0xa5: rzp(lda); break;
			case 0xa6: rzp(ldx); break;
			case 0xa7: mzp([](u8 d) { return d | 4; }); break;
			case 0xa8: fld(y = a); clock += 2; break; // tay
			case 0xa9: imm(lda); break;
			case 0xaa: fld(x = a); clock += 2; break; // tax
			case 0xab: clock++; break;
			case 0xac: rabs(ldy); break;
			case 0xad: rabs(lda); break;
			case 0xae: rabs(ldx); break;
			case 0xaf: rzp([&](u8 d) { br(d & 4); }); break;
			case 0xb0: br(CY); break; // bcs
			case 0xb1: rindy(lda); break;
			case 0xb2: rzpp(lda); break;
			case 0xb3: clock++; break;
			case 0xb4: rzpx(ldy); break;
			case 0xb5: rzpx(lda); break;
			case 0xb6: rzpy(ldx); break;
			case 0xb7: mzp([](u8 d) { return d | 8; }); break;
			case 0xb8: fclv(); clock += 2; break; // clv
			case 0xb9: rabsy(lda); break;
			case 0xba: fld(x = s); clock += 2; break;
			case 0xbb: clock++; break;
			case 0xbc: rabsx(ldy); break;
			case 0xbd: rabsx(lda); break;
			case 0xbe: rabsy(ldx); break;
			case 0xbf: rzp([&](u8 d) { br(d & 8); }); break;
			case 0xc0: imm(cpy); break;
			case 0xc1: rindx(cmp); break;
			case 0xc2: pc++; clock += 2; break;
			case 0xc3: clock++; break;
			case 0xc4: rzp(cpy); break;
			case 0xc5: rzp(cmp); break;
			case 0xc6: mzp(dec); break;
			case 0xc7: mzp([](u8 d) { return d | 0x10; }); break;
			case 0xc8: fld(++y); clock += 2; break; // iny
			case 0xc9: imm(cmp); break;
			case 0xca: fld(--x); clock += 2; break; // dex
			case 0xcb: waitflags |= W_WAI; pc--; return 0;
			case 0xcc: rabs(cpy); break;
			case 0xcd: rabs(cmp); break;
			case 0xce: mabs(dec); break;
			case 0xcf: rzp([&](u8 d) { br(d & 0x10); }); break;
			case 0xd0: br(!(p & MZ)); break; // bne
			case 0xd1: rindy(cmp); break;
			case 0xd2: rzpp(cmp); break;
			case 0xd3: clock++; break;
			case 0xd4: pc++; clock += 4; break;
			case 0xd5: rzpx(cmp); break;
			case 0xd6: mzpx(dec); break;
			case 0xd7: mzp([](u8 d) { return d | 0x20; }); break;
			case 0xd8: fcld(); clock += 2; break;
			case 0xd9: rabsy(cmp); break;
			case 0xda: st8(0x100 | s--, x); clock += 3; break;
			case 0xdb: waitflags |= W_STP; return 0;
			case 0xdc: pc += 2; clock += 4; break;
			case 0xdd: rabsx(cmp); break;
			case 0xde: mabsx(dec); break;
			case 0xdf: rzp([&](u8 d) { br(d & 0x20); }); break;
			case 0xe0: imm(cpx); break;
			case 0xe1: rindx(sbc); break;
			case 0xe2: pc++; clock += 2; break;
			case 0xe3: clock++; break;
			case 0xe4: rzp(cpx); break;
			case 0xe5: rzp(sbc); break;
			case 0xe6: mzp(inc); break;
			case 0xe7: mzp([](u8 d) { return d | 0x40; }); break;
			case 0xe8: fld(++x); clock += 2; break; // inx
			case 0xe9: imm(sbc); break;
			case 0xea: clock += 2; break;
			case 0xeb: clock++; break;
			case 0xec: rabs(cpx); break;
			case 0xed: rabs(sbc); break;
			case 0xee: mabs(inc); break;
			case 0xef: rzp([&](u8 d) { br(d & 0x40); }); break;
			case 0xf0: br(p & MZ); break; // beq
			case 0xf1: rindy(sbc); break;
			case 0xf2: rzpp(sbc); break;
			case 0xf3: clock++; break;
			case 0xf4: pc++; clock += 4; break;
			case 0xf5: rzpx(sbc); break;
			case 0xf6: mzpx(inc); break;
			case 0xf7: mzp([](u8 d) { return d | 0x80; }); break;
			case 0xf8: fsed(); clock += 2; break; // sed
			case 0xf9: rabsy(sbc); break;
			case 0xfa: fld(x = ld8(0x100 | ++s)); clock += 4; break;
			case 0xfb: clock++; break;
			case 0xfc: pc += 2; clock += 4; break;
			case 0xfd: rabsx(sbc); break;
			case 0xfe: mabsx(inc); break;
			case 0xff: rzp([&](u8 d) { br(d & 0x80); }); break;
		}
#if W65C02_TRACE
		tracep->p = p;
		tracep->a = a;
		tracep->x = x;
		tracep->y = y;
		tracep->s = s;
#if W65C02_TRACE > 1
		if (++tracep >= tracebuf + TRACEMAX - 1) StopTrace();
#else
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
#endif
#endif
	} while (clock < n);
	return clock - n;
}

template<int M> W65C02::u16 W65C02::fset(u8 a, u8 d, u8 s) {
	if constexpr ((M & 0xf) == C0) p &= ~MC;
	if constexpr ((M & 0xf) == C1) p |= MC;
	if constexpr ((M & 0xf) == CADD) p = ((s & d) | (~a & d) | (s & ~a)) & 0x80 ? p | MC : p & ~MC;
	if constexpr ((M & 0xf) == CSUB) p = ((s & ~d) | (a & ~d) | (s & a)) & 0x80 ? p & ~MC : p | MC;
	if constexpr ((M & 0xf) == CLEFT) p = d & 0x80 ? p | MC : p & ~MC;
	if constexpr ((M & 0xf) == CRIGHT) p = d & 1 ? p | MC : p & ~MC;
	if constexpr ((M & 0xf0) == Z8) p = a ? p & ~MZ : p | MZ;
	if constexpr ((M & 0xf000) == D0) p &= ~MD;
	if constexpr ((M & 0xf000) == D1) p |= MD;
	if constexpr ((M & 0xf000000) == V0) p &= ~MV;
	if constexpr ((M & 0xf000000) == VD) p = d & MV ? p | MV : p & ~MV;
	if constexpr ((M & 0xf000000) == VADD) p = ((d & s & ~a) | (~d & ~s & a)) & 0x80 ? p | MV : p & ~MV;
	if constexpr ((M & 0xf000000) == VSUB) p = ((d & ~s & ~a) | (~d & s & a)) & 0x80 ? p | MV : p & ~MV;
	if constexpr ((M & 0xf0000000) == ND) p = d & MN ? p | MN : p & ~MN;
	if constexpr ((M & 0xf0000000) == N8) p = a & 0x80 ? p | MN : p & ~MN;
	return a;
}

#if W65C02_TRACE
#include <string>
void W65C02::StopTrace() {
	TraceBuffer *endp = tracep;
	int i = 0, j;
	FILE *fo;
	if (!(fo = fopen((std::string(getenv("HOME")) + "/Desktop/trace.txt").c_str(), "w"))) exit(1);
	do {
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
		fprintf(fo, "%4d %04X  ", i++, tracep->pc);
		for (j = 0; j < tracep->opn; j++) fprintf(fo, "%02X ", tracep->op[j]);
		for (; j < OPMAX; j++) fprintf(fo, "   ");
		fprintf(fo, "%02X %02X %02X %02X %c%c%c%c%c%c%c%c ",
				tracep->a, tracep->x, tracep->y, tracep->s,
				tracep->p & 0x80 ? 'N' : '-',
				tracep->p & 0x40 ? 'V' : '-',
				tracep->p & 0x20 ? '+' : '-',
				tracep->p & 0x10 ? 'B' : '-',
				tracep->p & 0x08 ? 'D' : '-',
				tracep->p & 0x04 ? 'I' : '-',
				tracep->p & 0x02 ? 'Z' : '-',
				tracep->p & 0x01 ? 'C' : '-');
		for (Acs *p = tracep->acs; p < tracep->acs + tracep->index; p++) {
			switch (p->type) {
				case acsLoad8:
					fprintf(fo, "L %04X %02X ", p->adr, p->data & 0xff);
					break;
				case acsLoad16:
					fprintf(fo, "L %04X %04X ", p->adr, p->data);
					break;
				case acsStore8:
					fprintf(fo, "S %04X %02X ", p->adr, p->data & 0xff);
					break;
				case acsStore16:
					fprintf(fo, "S %04X %04X ", p->adr, p->data);
					break;
			}
		}
		fprintf(fo, "\n");
	} while (tracep != endp);
	fclose(fo);
	fprintf(stderr, "trace dumped.\n");
	exit(1);
}
#endif	// W65C02_TRACE
