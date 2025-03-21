// W65C02
// Copyright 2022-2025 © Yasuo Kuwahara
// MIT License

#include "RW.h" // Catakig
#include <cstdint>

#define W65C02_TRACE	0

#if W65C02_TRACE
#define W65C02_TRACE_LOG(adr, data, type) \
	if (tracep->index < ACSMAX) tracep->acs[tracep->index++] = { adr, (u16)data, type }
#else
#define W65C02_TRACE_LOG(adr, data, type)
#endif

class W65C02 : public RW /* <- superclass for Catakig */ {
	using s8 = int8_t;
	using u8 = uint8_t;
	using u16 = uint16_t;
	enum { M_IRQ = 1, M_NMI };
public:
	W65C02();
	void Reset();
	int Execute(int n);
	u16 GetPC() const { return pc; }
	void SetPC(u16 _pc) { pc = _pc; } // Catakig
	void IRQ() { irq |= M_IRQ; }
	void NMI() { irq |= M_NMI; }
private:
	// customize for Catakig -- start
	u8 imm8() {
		u8 data = READ(pc++);
#if W65C02_TRACE
		if (tracep->opn < OPMAX) tracep->op[tracep->opn++] = data;
#endif
		return data;
	}
	u16 imm16() {
		u8 data0 = READ(pc++), data1 = READ(pc++);
#if W65C02_TRACE
		if (tracep->opn < OPMAX) tracep->op[tracep->opn++] = data0;
		if (tracep->opn < OPMAX) tracep->op[tracep->opn++] = data1;
#endif
		return data0 | data1 << 8;
	}
	u8 ld8(u16 adr) {
		u8 data = Read(adr);
		W65C02_TRACE_LOG(adr, data, acsLoad8);
		return data;
	}
	u16 ld16(u16 adr) {
		u16 data = Read(adr);
		data |= Read(adr + 1) << 8;
		W65C02_TRACE_LOG(adr, data, acsLoad16);
		return data;
	}
	void st8(u16 adr, u8 data) {
		Write(adr, data);
		W65C02_TRACE_LOG(adr, data, acsStore8);
	}
	// customize for Catakig -- end
	template<typename F> void imm(F func) { func(imm8()); clock += 2; }
	template<typename F> void ma(F func) { a = func(a); clock += 2; }
	template<typename F> void rabs(F func) { func(ld8(imm16())); clock += 4; }
	template<typename F> void wabs(F func) { st8(imm16(), func()); clock += 4; }
	template<typename F> void mabs(F func) { u16 t = imm16(); st8(t, func(ld8(t))); clock += 6; }
	template<typename F> void rzp(F func) { func(ld8(imm8())); clock += 3; }
	template<typename F> void wzp(F func) { st8(imm8(), func()); clock += 3; }
	template<typename F> void mzp(F func) { u16 t = imm8(); st8(t, func(ld8(t))); clock += 5; }
	template<typename F> void rindx(F func) { func(ld8(ld16(imm8() + x & 0xff))); clock += 6; }
	template<typename F> void windx(F func) { st8(ld16(imm8() + x & 0xff), func()); clock += 6; }
	template<typename F> void rindy(F func) {
		u16 adr0 = ld16(imm8()), adr = adr0 + y;
		func(ld8(adr));
		clock += 5 + ((adr0 & 0xff00) != (adr & 0xff00));
	}
	template<typename F> void windy(F func) { st8(ld16(imm8()) + y, func()); clock += 6; }
	template<typename F> void rzpx(F func) { func(ld8(imm8() + x & 0xff)); clock += 4; }
	template<typename F> void wzpx(F func) { st8(imm8() + x & 0xff, func()); clock += 4; }
	template<typename F> void mzpx(F func) { u16 t = imm8() + x & 0xff; st8(t, func(ld8(t))); clock += 6; }
	template<typename F> void rzpy(F func) { func(ld8(imm8() + y & 0xff)); clock += 4; }
	template<typename F> void wzpy(F func) { st8(imm8() + y & 0xff, func()); clock += 4; }
	template<typename F> void rabsx(F func) {
		u16 adr0 = imm16(), adr = adr0 + x;
		func(ld8(adr));
		clock += 4 + ((adr0 & 0xff00) != (adr & 0xff00));
	}
	template<typename F> void wabsx(F func) { st8(imm16() + x, func()); clock += 5; }
	template<typename F> void mabsx(F func) { u16 adr = imm16() + x; st8(adr, func(ld8(adr))); clock += 7; }
	template<typename F> void rabsy(F func) {
		u16 adr0 = imm16(), adr = adr0 + y;
		func(ld8(adr));
		clock += 4 + ((adr0 & 0xff00) != (adr & 0xff00));
	}
	template<typename F> void wabsy(F func) { st8(imm16() + y, func()); clock += 5; }
	template<typename F> void rzpp(F func) { func(ld8(ld16(imm8()))); clock += 5; }
	template<typename F> void wzpp(F func) { st8(ld16(imm8()), func()); clock += 5; }
	template<int M> u16 fset(u8 a = 0, u8 d = 0, u8 s = 0);
	u8 a, x, y, s, p;
	u8 irq, waitflags;
	u16 pc;
	int clock;
#if W65C02_TRACE
	static constexpr int TRACEMAX = 10000;
	static constexpr int ACSMAX = 2;
	static constexpr int OPMAX = 3;
	enum {
		acsStore8 = 4, acsStore16, acsLoad8, acsLoad16
	};
	struct Acs {
		u16 adr, data;
		u8 type;
	};
	struct TraceBuffer {
		u8 a, x, y, s, p;
		u16 pc;
		Acs acs[ACSMAX];
		u8 op[OPMAX];
		u8 index, opn;
	};
	TraceBuffer tracebuf[TRACEMAX];
	TraceBuffer *tracep;
public:
	void StopTrace();
#endif
};
