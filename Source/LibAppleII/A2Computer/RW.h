#include "CompareProcess.h"

struct A2Memory;

struct RW : CompareProcess {
	void SetA2(void *p, A2Memory *m) { a2 = p; memory = m; }
	void Remap(uint32_t sel);
	uint32_t READ(uint16_t adr) {
		uint32_t data;
#if defined(CURMPU) && defined(NEWMPU)
		if (ReadStart(adr, data)) return data;
#endif
		data = (zp + adr)[rmap[adr >> 9] << 11];
#if defined(CURMPU) && defined(NEWMPU)
		ReadEnd(data);
#endif
		return data;
	}
	void WRITE(uint16_t adr, uint8_t data) {
#if defined(CURMPU) && defined(NEWMPU)
		if (WriteStart(adr, data)) return;
#endif
		(zp + adr)[wmap[adr >> 9] << 11] = data;
	}
	uint32_t FLOATER(int32_t t) {
		return zp[0x400 + t]; // no log because of nesting
	}
	uint32_t RZ(uint16_t adr) {
		uint32_t data;
#if defined(CURMPU) && defined(NEWMPU)
		if (ReadStart(adr, data)) return data;
#endif
		data = zp[adr];
#if defined(CURMPU) && defined(NEWMPU)
		ReadEnd(data);
#endif
		return data;
	}
	void WZ(uint16_t adr, uint8_t data) {
#if defined(CURMPU) && defined(NEWMPU)
		if (WriteStart(adr, data)) return;
#endif
		zp[adr] = data;
	}
	uint32_t Read(uint16_t adr);
	void Write(uint16_t adr, uint8_t data);
	uint32_t PULL();
	void PUSH(uint8_t data);
	void SwitchMPU(bool f) { setCompare(f); }
	void Stop();
	void *a2;
	uint8_t *zp;
	int8_t *rmap, *wmap;
	A2Memory *memory;
};
