;-------------------------------------------------------------------------
;
; 2021 / 01 / 20
;
; Spiral - Спираль
;
;-------------------------------------------------------------------------

.data ; #region
	; растояния (м) #region
		Re		real10 6.371E+6			; экваториальный радиус
		Alt		real10 2.0E+5			; высота над экватором
	; #endregion
	; скорости (м/с) #region
		Vrm		real10 7.0E+4			; скорость истечения реактивной массы
	; #endregion
	; массы (кг) #region
		m0		real10 25.0E+3			; стартовая масса
	; #endregion
	; мошности (Дж) #region
		N0		real10 1.0E+6			; мошность двигателя
	; #endregion
	; константы #region
		mu		real10 398600.44158E+9	; гравитационный параметр
		div2	dd	2					; делитель 2
		mul4	dd	4
	; #endregion
; #endregion

.data? ; #region
	; растояние #region
		rp		real10 ?				; радиус-вектор
	; #endregion
	; время (с) #region
		Td		real10 ?				; временной интервал
	; #endregion
	; скорости (м/с) #region
		Vd		real10 ?				; дельта скорости
		V0		real10 ?				; начальная скорость
	; #endregion
	; массы (кг) #region
		mc		real10 ?				; масса после импульса
		mrtd	real10 ?				; расход реактивной масса
	; #endregion
; #endregion

.code
WinMain proc
	; инициализация #region
		; повыщенная точность / округление к нулю #region
		fnstcw [esp -2]
		or word ptr[esp -2], 0F00h
		fldcw [esp -2]
		; #endregion

		; радиус-вектор перигея (rp) #region
		fldpi					; r7 = pi
		fld		mu				; r6 = mu
		fld		Alt				; r5 = Alt
		fld		Re				; r4 = Re
		fadd					; r5 = r5 + r4 = Re + Alt = rp
		fmul	st(2),st(0)		; r7 = r7 * r5 = pi * rp
		fld		st(0)			; r4 = r5 = rp
		fstp	rp
		; #endregion
		; r5 = rp
		; r6 = mu
		; r7 = pi * rp

		; скорость до импульса (V0) #region
		fld		st(1)			; r4 = r6 = mu
		fdiv	st(0),st(1)		; r4 = r4 / r5 = mu / rp = V0^2
		fsqrt					; r4 = V0
		fld		st(0)			; r3 = V0
		fstp	V0
		; #endregion
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = pi * rp

		; продолжительность импульса (Td) #region
		fld		st(0)			; r3 = V0
		fimul	mul4			; r3 = V0 * 4 = 4V0
		fdivp	st(4),st(0)		; r7 = r7 / r3 = (pi * rp) / 4V0 = Td
		fld		st(3)			; r3 = Td
		fstp	Td				; Td = r3
		; #endregion
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; расход реактивной масса в секунду (mrt1) #region
		fld		Vrm				; r3 = Vrm
		fld		st(0)			; r2 = r3 = Vrm
		fmul	st(0),st(0)		; r2 = Vrm * Vrm = Vrm^2
		fld		N0				; r1 = N0
		fadd	st(0),st(0)		; r1 = N0 + N0 = 2N0
		fdivr					; r2 = r1 / r2 = 2N0 / Vrm^2 = mrt1
		; #endregion
		; r2 = mrt1
		; r3 = Vrm
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; расход реактивной масса в период (mrtd) #region
		fmul	st(0),st(5)		; r2 = r2 * r7 = mrt1 * Td = mrtd
		fld		st(0)			; r1 = r2 = mrtd
		fstp	mrtd			; mrtd = r1
		; #endregion
		; r2 = mrtd
		; r3 = Vrm
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td

	; #endregion
restart:
	; начальная (первая) точка #region

		; текущая масса (mc) часть 1 #region
		fld1					; r1 = 1.0
		fld		m0				; r0 = m0
		fsubr	st(2),st(0)		; r2 = r0 - r2 = m0 - mrtd = mc
		; #endregion
		; r0 = m0
		; r1 = 1.0
		; r2 = mc
		; r3 = Vrm
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td
	
		; дельта скорости (Vd) часть 1 #region
		fdiv	st(0),st(2)		; r0 = r0 / r2 = (m0 / mc)
		fyl2x					; r1 = log2(m0 / mc)
		fldl2e					; r0 = log2(e)
		fdivp					; r1 = r1 / r0 = log2(m0 / mc) / log2(e) = ln(m0 / mc)
		fmulp	st(2),st(0)		; r3 = r1 / r3 = ln(m0 / mc) * Vrm = Vd
		; #endregion
		; r2 = mc
		; r3 = Vd
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; текущая масса (mc) часть 2 #region
		fstp	mc				; mc = r2
		; #endregion
		; r3 = Vd
		; r4 = V0
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; скорость после импульса (V1) #region
		fadd	st(1),st(0)		; r4 = r4 + r3 = V0 + Vd = V1
		; #endregion
		; r3 = Vd
		; r4 = V1
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; дельта скорости (Vd) часть 2 #region
		fstp	Vd				; Vd = r3
		; #endregion
		; r4 = V1
		; r5 = rp
		; r6 = mu
		; r7 = Td
	
		; экцентриситет (e) #region
		fmul	st(0),st(0)		; r4 = V1 * V1 = V1^2
		fdiv	st(0),st(2)		; r4 = r4 / r6 = V1^2 / mu = (1.0 + e)^2 / p
		fmul	st(0),st(1)		; r4 = r4 * r5 = (1.0 + e)^2 / p * rp = (1.0 + e)
		fld1					; r3 = 1.0
		fsub	st(1),st(0)		; r4 = r4 - r3 = (1.0 + e) - 1.0 = e
		; #endregion
		; r3 = 1.0
		; r4 = e
		; r5 = rp
		; r6 = mu
		; r7 = Td

		; большая полуось (a) #region
		fsub	st(0),st(1)		; r3 = r3 - r4 = (1.0 - e)
		fdivp	st(2),st(0)		; r5 = r5 / r3 = rp / (1.0 - e) = a
		; #endregion
		; r4 = e
		; r5 = a
		; r6 = mu
		; r7 = Td

		; средния аномалия (M) #region
		fld		st(1)			; r3 = r5 = a
		fmul	st(0),st(0)		; r3 = a * a = a^2
		fmul	st(0),st(2)		; r3 = r3 * r5 = a^2 * a = a^3
		fdivr	st(0),st(3)		; r3 = r6 / r3 = mu / a^3 = n^2
		fsqrt					; r3 = n
		fmulp	st(4),st(0)		; r7 = r7 * r3 = Td * n = M
		; #endregion
		; r4 = e
		; r5 = a
		; r6 = mu
		; r7 = M

	; #endregion
next:
	; параметры до импульса #region 

		; экцентрическая аномалия (E0) #region
		fld1					; r3 = 1.0
		fld		st(4)			; r2 = r7 = M
		fsincos					; r1 = cos(M)
								; r2 = sin(M)
		fmul	st(0),st(3)		; r1 = r1 * r4 = cos(M) * e
		fsubp	st(2),st(0)		; r3 = r3 - r1 = 1.0 - cos(M) * e
		fmul	st(0),st(2)		; r2 = r2 * r4 = sin(M) * e
		fdivrp					; r3 = r2 / r3 = (sin(M) * e) / (1.0 - cos(M) * e)
		fsubr	st(0),st(4)		; r3 = r7 - r3 = M - (sin(M) * e) / (1.0 - cos(M) * e) = E0
		fstp	st(3)			; r6 = r3 = E0
		fldz
		; #endregion
		; r3 = 0.0
		; r4 = e
		; r5 = a
		; r6 = E0
		; r7 = M

	@@:	; проверка (M == (En - sin(En) * e)) #region
		fld		st(3)			; r2 = r6 = En
		fsin					; r2 = sin(En)
		fmul	st(0),st(2)		; r2 = r2 * r4 = sin(En) * e
		fsubr	st(0),st(4)		; r2 = r6 - r2 = En - sin(En) * e
		fsub	st(0),st(5)		; r2 = r2 - r7 = En - sin(En) * e - M = dEn
		fabs					; r2 = [dEn]
		fcomi	st(0),st(1)
		fstp	st(1)			; r3 = dEn
		; #endregion
		; r3 = dEn
		; r4 = e
		; r5 = a
		; r6 = En
		; r7 = M
		je @f

		; экцентрическая аномалия (En) #region
		fld1					; r2 = 1.0
		fld		st(4)			; r1 = r6 = En
		fsincos					; r0 = cos(En)
								; r1 = sin(En)
		fmul	st(0),st(4)		; r0 = r0 * r4 = cos(En) * e
		fsubp	st(2),st(0)		; r2 = r2 - r0 = 1.0 - cos(En) * e
		fmul	st(0),st(3)		; r1 = r1 * r4 = sin(En) * e
		fadd	st(0),st(6)		; r1 = r1 + r7 = sin(En) * e + M
		fsubr	st(0),st(5)		; r1 = r6 - r1 = En - (sin(En) * e + M)
		fdivrp	st(1),st(0)		; r2 = r1 / r2 = (En - sin(En) * e - M) / (1.0 - cos(En) * e) = dE(n + 1)
		fsubp	st(4),st(0)		; r6 = r6 - r2 = En - dE(n + 1) = E(n+1)
		; #endregion
		; r3 = dEn
		; r4 = e
		; r5 = a
		; r6 = E(n+1)
		; r7 = M
		jmp @b

	@@:	; выход из первого квадранта #region
		fstp	dword ptr[esp - 4]
		fldpi					; r3 = pi
		fidiv	div2			; r3 = pi / 2
		fcomip	st(0),st(3)		; pi / 2 > En
		; #endregion
		; r4 = e
		; r5 = a
		; r6 = En
		; r7 = M
		ja @f

		; рестарт #region
		finit
		fld		Td				; r7 = Td0
		fidiv	div2			; r7 = Td0 / 2 = Td
		fld		st(0)			; r6 = r7 = Td
		fstp	Td				; Td = r6
		fld		mu				; r6 = mu
		fld		rp				; r5 = rp
		fld		V0				; r4 = V0
		fld		Vrm				; r3 = Vrm
		fld		mrtd			; r2 = mrtd
		; #endregion
		jmp restart

	@@:	; катит истиной аномалии (X^2) #region
		fld		st(2)			; r3 = r6 = En
		fsincos					; r2 = cos(En)
								; r3 = sin(En)
		fsub	st(0),st(2)		; r2 = r2 - r4 = cos(En) - e
		fmul	st(0),st(3)		; r2 = r2 * r5 = (cos(En) - e) * a = X
		fmul	st(0),st(0)		; r2 = X * X = X^2
		fstp	st(5)			; r7 = r2 = X^2
		; #endregion
		; r3 = sin(En)
		; r4 = e
		; r5 = a
		; r6 = En
		; r7 = X^2

		; фокальный параметр (p) #region
		fmul	st(0),st(2)		; r3 = r3 * r5 = sin(En) * a
		fmul	st(0),st(0)		; r3 = (sin(En) * a)^2
		fld		st(1)			; r2 = r4 = e
		fmulp	st(2),st(0)		; r4 = r4 * r2 = e * e = e^2
		fld1					; r2 = 1.0
		fsub	st(0),st(2)		; r2 = r2 - r4 = (1.0 - e^2)
		fmul	st(3),st(0)		; r5 = r5 * r2 = a * (1.0 - e^2) = p
		; #endregion
		; r2 = (1.0 - e^2)
		; r3 = (sin(En) * a)^2
		; r4 = e^2
		; r5 = p
		; r6 = En
		; r7 = X^2

		; катит истиной аномалии (Y^2) #region
		fmulp					; r3 = r3 * r2 = (sin(En) * a)^2 * (1.0 - e^2) = Y^2
		; #endregion
		; r3 = Y^2
		; r4 = e^2
		; r5 = p
		; r6 = En
		; r7 = X^2

		; квадрат радиус-вектора (r^2) #region
		fadd	st(4),st(0)		; r7 = r7 + r3 = X^2 + Y^2 = r^2
		; #endregion
		; r3 = Y^2
		; r4 = e^2
		; r5 = p
		; r6 = En
		; r7 = r^2

		; квадрат синуса (sin^2(theta)) #region
		fdiv	st(0),st(4)		; r3 = r3 / r7 = Y^2 / r^2 = sin^2(theta)
		; #endregion
		; r3 = sin^2(theta)
		; r4 = e^2
		; r5 = p
		; r6 = En
		; r7 = r^2

		; квадрат радиальной скорости (Vr^2 / mu) #region
		fmulp					; r4 = r4 * r3 = e^2 * sin^2(theta)
		fdiv	st(0),st(1)		; r4 = r4 * r5 = (e^2 * sin^2(theta)) / p = Vr^2 / mu
		fstp	st(2)			; r6 = r4 = Vr^2 / mu
		; #endregion
		; r5 = p
		; r6 = Vr^2 / mu
		; r7 = r^2

		; квадрат поперечная скорость (Vn^2 / mu) #region
		fdiv	st(0),st(2)		; r5 = r5 * r7 = p / r^2 = Vn^2 / mu
		; #endregion
		; r5 = Vn^2 / mu
		; r6 = Vr^2 / mu
		; r7 = r^2

		; квадрат скорости (V^2 / mu) #region
		fld		st(0)			; r4 = r5 = Vn^2 / mu
		fadd	st(0),st(2)		; r4 = r4 + r6 = Vn^2 / mu + Vr^2 / mu = V0^2 / mu
		; #endregion
		; r4 = V0^2 / mu
		; r5 = Vn^2 / mu
		; r6 = Vr^2 / mu
		; r7 = r^2
	
		; квадрат косинуса (cos^2(phi)) #region
		fdiv	st(2),st(0)		; r6 = r6 / r4 = (Vr^2 / mu) / (V0^2 / mu) = cos^2(phi)
		; #endregion
		; r4 = V0^2 / mu
		; r5 = Vn^2 / mu
		; r6 = cos^2(phi)
		; r7 = r^2

		; квадрат косинуса (sin^2(phi)) #region
		fdiv	st(1),st(0)		; r5 = r5 / r4 = (Vn^2 / mu) / (V0^2 / mu) = sin^2(phi)
		; #endregion
		; r4 = V0^2 / mu
		; r5 = sin^2(phi)
		; r6 = cos^2(phi)
		; r7 = r^2

	; #endregion

	; параметры после имупульса #region

		; орбитальная скорость (V) #region
		fld		mu				; r3 = mu
		fdiv	st(2),st(0)		; r5 = r5 / r3 = sin^2(phi) / mu
		fdiv	st(3),st(0)		; r6 = r6 / r3 = cos^2(phi) / mu
		fmul	st(0),st(1)		; r3 = r3 * r4 = mu * V0^2 / mu = V0^2
		fdivr	st(1),st(0)		; r4 = r3 / r4 = V0^2 / (V0^2 / mu) = mu
		fsqrt					; r3 = V0
		; #endregion
		; r3 = V0
		; r4 = mu
		; r5 = sin^2(phi) / mu
		; r6 = cos^2(phi) / mu
		; r7 = r^2
		
		; текущая масса (mc) #region
		fld1					; r2 = 1.0
		fld		mc				; r1 = mc0
		fld		mrtd			; r0 = mctd
		fsubr	st(0),st(1)		; r0 = r1 - r0 = mc0 - mctd = mc
		; #endregion
		; r0 = mc
		; r1 = mc0
		; r2 = 1.0
		; r3 = V0
		; r4 = mu
		; r5 = Vn^2 / mu
		; r6 = cos^2(phi) / mu
		; r7 = r^2

		; дельта скорости (Vd) #region
		fdiv	st(1),st(0)		; r1 = r1 / r0 = mc0 / mc
		fstp	mc				; mc = r0
		fyl2x					; r2 = log2(mc0 / mc)
		fldl2e					; r1 = log2(e)
		fdivp					; r2 = r2 / r1 = log2(mc0 / mc) / log2(e) = ln(mc0 / mc)
		fld		Vrm				; r1 = Vrm
		fmulp					; r2 = r2 * r1 = ln(mc0 / mc) * Vrm = Vd
		fld		Vd				; r1 = Vd(n-1)
		fadd	st(0),st(1)		; r1 = r1 + r2 = Vd(n-1) + Vd
		fstp	Vd
		; #endregion
		; r2 = Vd
		; r3 = V0
		; r4 = mu
		; r5 = sin^2(phi) / mu
		; r6 = cos^2(phi) / mu
		; r7 = r^2

		; #region
		fld		st(1)			; r1 = V0
		fidiv	div2			; r1 = V0 / 2
		fcomip	st(0),st(1)
		; #endregion
		jb exit

		; квадрат скорость после импульса (V^2) #region
		faddp	st(1),st(0)		; r3 = r3 + r2 = Vp + Vd = V
		fmul	st(0),st(0)   	; r3 = V^2
		; #endregion
		; r3 = V^2
		; r4 = mu
		; r5 = sin^2(phi) / mu
		; r6 = cos^2(phi) / mu
		; r7 = r^2
	
		; квадрат радиальной скорости (Vr^2 / mu) #region
		fmul	st(3),st(0)		; r6 = r6 * r3 = cos^2(phi) / mu * V^2 = Vr^2 / mu
		; #endregion
		; r3 = V^2
		; r4 = mu
		; r5 = sin^2(phi) / mu
		; r6 = Vr^2 / mu
		; r7 = r^2

		; фокальный параметр (p) #region
		fmul	st(0),st(4)		; r3 = r3 * r7 = V^2 * r^2
		fmul	st(0),st(2)		; r3 = r3 * r5 = V^2 * r^2 * (sin^2(phi) / mu) = p
		fmul	st(3),st(0)		; r5 = r5 * r3 = Vr^2 / mu = Vr^2 * p / mu
		fstp	st(2)			; r5 = r3 = p
		; #endregion
		; r4 = mu
		; r5 = p
		; r6 = Vr^2 * p / mu
		; r7 = r^2

		; квадрат экцентриситета (e^2) #region
		fld		st(3)			; r3 = r7 = r^2
		fsqrt					; r3 = r
		fsubr	st(0),st(2)		; r3 = r5 - r3 = (p - r)
		fmul	st(0),st(0)		; r3 = (p - r) * (p - r) = (p - r)^2
		fdiv	st(0),st(4)		; r3 = r3 / r7 = (p - r)^2 / r^2
		fadd	st(3),st(0)		; r6 = r6 + r3 = V1r^2 * p / mu + (p - r)^2 / r^2 = e^2
		; #endregion
		; r3 = (p - r)^2 / r^2
		; r4 = mu
		; r5 = p
		; r6 = e^2
		; r7 = r^2

		; квадрат косинус истинной аномалии (cos^2(theta)) #region
		fdiv	st(0),st(3)		; r3 = r3 / r6 = ((p - r)^2 / r^2) / e^2 = cos^2(theta)
		; #endregion
		; r3 = cos^2(theta)
		; r4 = mu
		; r5 = p
		; r6 = e^2
		; r7 = r^2

		; квадрат координаты экцентрической аномалии (Y^2) #region
		fmul	st(0),st(4)		; r3 = r3 * r7 = cos^2(theta) * r^2
		fld		st(0)			; r2 = r3 = cos^2(theta) * r^2
		fsubp	st(5),st(0)		; r7 = r7 - r2 = r^2 - cos^2(theta) * r^2 = Y^2
		; #endregion
		; r3 = cos^2(theta) * r^2
		; r4 = mu
		; r5 = p
		; r6 = e^2
		; r7 = Y^2

		; большая полуось (a) #region
		fld1					; r2 = 1.0
		fsub	st(0),st(4)		; r2 = r2 - r6 = (1.0 - e^2)
		fdivp	st(3),st(0)		; r5 = r5 / r2 = p / (1.0 - e^2) = a
		; #endregion
		; r3 = cos^2(theta) * r^2
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = Y^2

		; квадрат координаты экцентрической аномалии (X^2) #region
		fsqrt					; r3 = cos(theta) * r
		fld		st(3)			; r2 = r6 = e^2
		fsqrt					; r2 = e
		fmul	st(0),st(3)		; r2 = r2 * r5 = e * a
		faddp					; r3 = r3 + r2 = cos(theta) * r + e * a = X
		fmul	st(0),st(0)		; r3 = X * X = X^2
		; #endregion
		; r3 = X^2
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = Y^2

		; синус экцентрической аномалии (sin^2(E)) #region
		fadd	st(0),st(4)		; r3 = r3 + r7 = X^2 + Y^2 = r^2
		fdivp	st(4),st(0)		; r7 = r3 / r7 = Y^2 / r^2 = sin^2(E)
		; #endregion
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = sin^2(E)

		; экцентрическая аномалия (E0) #region
		fld		st(3)			; r3 = r7 = sin^2(E)
		fsqrt					; r3 = sin(E)
		fld1					; r2 = 1.0
		fsub	st(0),st(5)		; r2 = r2 - r7 = 1.0 - sin^2(E)
		fpatan					; r3 = E
		; #endregion
		; r3 = E
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = sin^2(E)

		; средния аномалия (M0) #region
		fld		st(4)			; r2 = r7 = sin^2(E)
		fmul	st(0),st(4)		; r2 = r2 * r6 = sin^2(E) * e^2
		fsqrt					; r2 = sin(E) * e
		fsubp					; r3 = r2 - r3 = E - sin(E) * e = M0
		; #endregion
		; r3 = M0
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = sin^2(E)

		; средние движение (n) #region
		fld		st(2)			; r2 = r5 = a
		fmul	st(0),st(0)		; r2 = a * a = a^2
		fmul	st(0),st(3)		; r2 = r2 * r5 = a^3
		fdivr	st(0),st(2)		; r2 = r4 / r2 = mu / a^3 = n^2
		fsqrt					; r2 = n
		; #endregion
		; r2 = n
		; r3 = M0
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = sin^2(E)

		; средния аномалия (M) #region
		fdiv	st(1),st(0)		; r3 = r3 / r2 = M0 / n = T0
		fld		Td				; r1 = Td
		faddp	st(2),st(0)		; r3 = r3 + r1 = T0 + Td = T
		fmulp					; r3 = r3 * r2 = T * n = M
		fstp	st(4)			; r7 = r3 = M
		; #endregion
		; r4 = mu
		; r5 = a
		; r6 = e^2
		; r7 = M

		; перестановка #region
		fxch	st(2)			; r2 = r2 = e^2
								; r4 = r2 = mu
		fsqrt					; r2 = e
		; #endregion
		; r4 = e
		; r5 = a
		; r6 = E0
		; r7 = M

	; #endregion
	jmp next
exit:
	xor	rax,rax
WinMain endp
end
