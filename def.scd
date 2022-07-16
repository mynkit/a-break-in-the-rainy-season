Server.killAll;

// 出力先デバイスの確認
ServerOptions.inDevices;
ServerOptions.outDevices;

// 出力先変更
(
Server.default.options.inDevice_(ServerOptions.inDevices[0]);
Server.default.options.outDevice_(ServerOptions.outDevices[0]);
Server.default.reboot;
SuperDirt.start;
)

(
Server.default.options.inDevice_("BlackHole 2ch");
Server.default.options.outDevice_("BlackHole 2ch");
Server.default.reboot;
SuperDirt.start;
)

// シンセやエフェクトの定義
(
var decoder = FoaDecoderMatrix.newStereo((100).degrad, (3-sqrt(3))/2);

~dirt.addModule('HRTF', { |dirtEvent|
	dirtEvent.sendSynth('HRTF' ++ ~dirt.numChannels,
		[
			theta: ~theta,
			phi: ~phi,
			out: ~out
		]
	)
}, { ~theta.notNil or: { ~phi.notNil }});
SynthDef("HRTF" ++ ~dirt.numChannels, { |out, theta, phi|
	var signal, in, left, right, foa;
	in = In.ar(out, ~dirt.numChannels);
	left = in[0];
	right = in[1];
	in = [(left+right), (left+right)];
	// theta is our angle on the X-Y plane and phi is our elevation
	theta = (theta-1) * pi;
	phi = phi * (-pi/2);
	// Encode into our foa signal
    foa = FoaPanB.ar(in, theta, phi);
	// decode our signal using our decoder defined above
    signal = FoaDecode.ar(foa, decoder);
	ReplaceOut.ar(out, signal)
}, [\ir]).add;

// Conformer
~dirt.addModule('spectral-conformer', { |dirtEvent|
	dirtEvent.sendSynth('spectral-conformer' ++ ~dirt.numChannels,
		[
			real: ~real,
			imag: ~imag,
			realHpf: ~realHpf,
			out: ~out
		]
	)
}, { ~real.notNil or: { ~imag.notNil } or: { ~realHpf.notNil } });

SynthDef("spectral-conformer" ++ ~dirt.numChannels, { |out, real = 0.5, imag = 0.5, realHpf = 100|
	var signal, chain, in, clean, teeth = 256;
	signal = In.ar(out, ~dirt.numChannels);
	clean = signal;
	chain = signal.asArray.collect { |x| FFT(LocalBuf(2048), x) };
	signal = IFFT(
			PV_ConformalMap(chain, real.linlin(0.0,1.0,0.01,2.0), imag.linlin(0.00,1.0,0.01,10.0))
		).tanh;
	signal = HPF.ar(signal, realHpf);
	ReplaceOut.ar(out, signal)
}, [\ir]).add;

// ~dirt.loadSoundFiles(thisProcess.nowExecutingPath.dirname ++ "/samples/*");

~dirt.orbits.do { |x|
	x.globalEffects = [
		GlobalDirtEffect(\dirt_delay, [\delaytime, \delayfeedback, \delaySend, \delayAmp, \lock, \cps]),
		GlobalDirtEffect(\pingpong_delay, [\pingpongt, \pingpongfb, \pingpong, \cps]),
		GlobalDirtEffect(\dirt_reverb, [\size, \room, \dry]),
		GlobalDirtEffect(\schroeder_reverb, [\scReverb, \ice]),
		GlobalDirtEffect(\freeverb, [\size, \freeverb, \damp]),
		GlobalDirtEffect(\dirt_leslie, [\leslie, \lrate, \lsize]),
		GlobalDirtEffect(\dirt_rms, [\rmsReplyRate, \rmsPeakLag]).alwaysRun_(true),
		GlobalDirtEffect(\dirt_monitor, [\limitertype]).alwaysRun_(true)
	]
};

SynthDef("pingpong_delay" ++ ~dirt.numChannels, { |dryBus, effectBus, gate = 1, pingpongt, pingpongfb, pingpong, cps = 1|
	var signal = In.ar(dryBus, ~dirt.numChannels);
	var maxDelayTime = 4;
	var delaytime, delayfeedback, delayAmp, decayTime, left, right, dry, pingponglpf=1000;

	delayAmp = pingpong;
	delaytime = pingpongt;
	delayfeedback = pingpongfb;

	delayfeedback = delayfeedback.clip(0, 0.99);
	delaytime = delaytime * reciprocal(cps);
	delaytime = delaytime.clip(0, maxDelayTime); // just to be sure
	decayTime = log2(-60.dbamp) / log2(delayfeedback) * delaytime;
	decayTime = decayTime.clip(0, 20);

	dry = signal;
	right = CombL.ar(dry, maxDelayTime*2, delaytime.lag(0.05)*2, decayTime);
	right = LPF.ar(right, pingponglpf);
	left = DelayN.ar(dry + right, maxDelayTime, delaytime.lag(0.05));
	signal = NumChannels.ar([dry, dry] + [left*delayAmp, right*delayAmp], ~dirt.numChannels);

	signal = signal * EnvGen.kr(Env.asr, gate, doneAction:2);

	DirtPause.ar(signal, graceTime:4);

	Out.ar(effectBus, signal);
}, [\ir, \ir]).add;

SynthDef("schroeder_reverb" ++ ~dirt.numChannels, { |dryBus, effectBus, gate = 1, scReverb, ice|
	var signal = In.ar(dryBus, ~dirt.numChannels);
	var chain, in, z, y, oct;

	z = DelayN.ar(signal, 0.048);
	y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, 1, 15) }));
	// 32.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
	32.do({ y = AllpassN.ar(y, 0.02, [0.02.rand, 0.02.rand], 1) });
	oct = 1.0 * LeakDC.ar( abs(y) );
	y = SelectX.ar(ice, [y, ice * oct, DC.ar(0)]);
	signal = signal + (0.2*y*scReverb);

	signal = signal * EnvGen.kr(Env.asr, gate, doneAction:2);

	DirtPause.ar(signal, graceTime:4);

	Out.ar(effectBus, signal);
}, [\ir, \ir]).add;

SynthDef("freeverb" ++ ~dirt.numChannels, { |dryBus, effectBus, gate = 1, size, freeverb, damp|
	var signal = In.ar(dryBus, ~dirt.numChannels);
	var y;

	y = FreeVerb.ar(signal, 1, size, damp);

	signal = signal + (y * freeverb);

	DirtPause.ar(signal, graceTime:4);

	Out.ar(effectBus, signal);
}, [\ir, \ir]).add;

Server.default.freeAll;

SynthDef("SinOscSynthBend", {
	arg out, sustain=1, decay=0, pan=0, freq=110, accelerate=0, slide=0, sustainpedal=0.5, vibratoFreq=10, vibratoDepth=0;
	var fglide, detune, sound, sig1, sig2, env, bendSustain;
	bendSustain = sustainpedal;
	// detune = 1.001;
	detune = 1.0;
	fglide = EnvGen.ar(Env.perc(0.05, 0.3, 1, \lin), timeScale:bendSustain) * (-1*slide/4) + 1;
	freq = freq * fglide * Line.kr(1, 1+accelerate, bendSustain);
	freq = freq *(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50)));
	sig1 = SinOsc.ar(freq).dup;
	sig2 = SinOsc.ar(freq * detune).dup;
	sound = (sig1 + sig2) * 0.5;
	env = EnvGen.ar(Env.linen(0.01, 0, 0.6, 1, -3), timeScale:sustain, doneAction:2);
	//env = EnvGen.ar(Env.pairs([[0,0],[0.03,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef("FastSinSynth", {
	arg out, sustain=1, decay=0, pan=0, freq=110, accelerate=0, vibratoFreq=10, vibratoDepth=0;
	var fglide, detune, sound, sig1, sig2, env, parFreq;
	detune = 1.0;
	freq = freq *(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50)));
	sig1 = SinOsc.ar(freq).dup;
	sig2 = SinOsc.ar(freq * detune).dup * 0.1;
	sound = (sig1 + sig2) * 0.5;
	parFreq = 0.8 + EnvGen.ar(Env.linen(0, 0, 0.6, 10, -7), timeScale:sustain);
	// pan = EnvGen.ar(Env.linen(0, 0, 0.6, (pan), 1), timeScale:sustain); // pan slide
	// sound = sound*LFPar.kr(parFreq); // tremoro
	// panning
	sound = Pan2.ar(sound, FSinOsc.kr(1.43));
	env = EnvGen.ar(Env.linen(0, 0, 0.6, 1, -2), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef("superhammondBend", {
	arg out, sustain=1, decay=0, pan, freq, vibrato=0.5, vrate=7, voice=0, perc=0, percf=2, accelerate=0, slide=0, sustainpedal=0.5;
	var freqs, amps, sound, click, env, ampscale, attrel, fglide, bendSustain;
	bendSustain = sustainpedal;
	// attrel = (sustain/2).clip(0,0.005);
	// env = EnvGen.ar(Env.linen(attrel, sustain-(2*attrel), attrel), doneAction:2);
	env = EnvGen.ar(Env.linen(0.1, sustain, 0.1, sustain), doneAction:2);
	fglide = EnvGen.ar(Env.perc(0.05, 0.3, 1, \lin), timeScale:bendSustain) * (-1*slide/4) + 1;
	freq = freq * fglide * Line.kr(1, 1+accelerate, bendSustain);
	freqs = [0.5, 1.4988, 1, 2, 2.9976, 4, 5.0409, 5.9953, 8, 1.001] * freq;
	freqs = 2**(freqs < 63) * freqs; // fold up bottom register
	freqs = freqs / (2**(freqs > 6000)); // fold down high harmonics
	freqs = freqs / (2**(freqs > 6000)); // do it again for the really high notes
	amps = SelectX.kr(voice, #[
		[1, 4, 5, 4, 3, 1, 0, 0, 0], // 0: bass violin 16'
		[0, 0, 8, 0, 4, 0, 0, 0, 0], // 1: tibia 8'
		[0, 7, 8, 1, 2, 0, 0, 0, 0], // 2: bassoon 8'
		[0, 0, 7, 8, 8, 8, 8, 7, 2], // 3: french trumpet 8'
		[4, 0, 5, 5, 4, 5, 3, 3, 6], // 4: string ensemble
		[8, 8, 5, 3, 2, 4, 5, 8, 8], // 5: Blues
		[8, 8, 8, 0 ,0, 0, 0, 0, 3], // 6: Jazz 1
		[8, 8, 8, 8, 8, 8, 8, 8, 8], // 7: Full Shout
		[8, 0, 0, 0, 0, 0, 8, 8, 8], // 8: Bro' Jack
		[0, 0, 8, 8, 0, 0, 0, 0, 0]  // 9: Jazz 2
	]);
	amps = amps - [0, 0, 0, 0, 0, 0, 0.5, 1, 1.5]; // damp brilliance a bit
	amps = 2**(amps-8);
	click = WhiteNoise.ar(1) * XLine.ar(1,1e-6, 0.1);
	sound = 0.5 * Mix.ar( SinOsc.ar(freqs, (2pi ! 9).rand, amps/amps.sum) );
	// オルガンサウンド特有のアタック音が欲しい場合
	//sound = 0.5 * Mix.ar( SinOsc.ar(freqs*(2*click+1), (2pi ! 9).rand, amps/amps.sum) );
	sound = perc * 0.5 * SinOsc.ar(freq*percf, 2pi.rand) * XLine.ar(1,1e-6,2*decay+1) + sound;
	sound = sound + BAllPass.ar(sound, 1000*LFTri.kr(vrate,0,vibrato/2,1));
	// panning
	sound = Pan2.ar(sound, FSinOsc.kr(1.43));
	Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef("bassSynth", {
	arg out, sustain=1, decay=0, pan, freq, vibrato=0.5, vrate=7, voice=0, perc=0, percf=2, accelerate=0, slide=0, sustainpedal=0.5;
	var freqs, amps, sound, click, env, ampscale, attrel, fglide, bendSustain;
	bendSustain = sustainpedal;
	attrel = (sustain/2).clip(0,0.005);
	env = EnvGen.ar(Env.linen(0, 0, 0.6, 1, 6), timeScale:sustain, doneAction:2);
	fglide = EnvGen.ar(Env.perc(0.05, 0.3, 1, \lin), timeScale:bendSustain) * (-1*slide/4) + 1;
	freq = freq * fglide * Line.kr(1, 1+accelerate, bendSustain);
	freqs = [0.5, 1.4988, 1, 2, 2.9976, 4, 5.0409, 5.9953, 8, 1.001] * freq;
	freqs = 2**(freqs < 63) * freqs; // fold up bottom register
	freqs = freqs / (2**(freqs > 6000)); // fold down high harmonics
	freqs = freqs / (2**(freqs > 6000)); // do it again for the really high notes
	amps = SelectX.kr(voice, #[
		[1, 4, 5, 4, 3, 1, 0, 0, 0], // 0: bass violin 16'
	]);
	amps = amps - [0, 0, 0, 0, 0, 0, 0.5, 1, 1.5]; // damp brilliance a bit
	amps = 2**(amps-8);
	click = WhiteNoise.ar(1) * XLine.ar(1,1e-6, 0.1);
	sound = 0.5 * Mix.ar( SinOsc.ar(freqs, (2pi ! 9).rand, amps/amps.sum) );
	// sound = perc * 0.5 * SinOsc.ar(freq*percf, 2pi.rand) * XLine.ar(1,1e-6,2*decay+1) + sound;
	// sound = sound + BAllPass.ar(sound, 1000*LFTri.kr(vrate,0,vibrato/2,1));
	// panning
	// sound = Pan2.ar(sound, FSinOsc.kr(1.43));
	Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef("MyMoog", {
	arg out, freq = 110, sustain = 1, pan = 0.5, pitch1=4.0, accelerate=0, slide=0, sustainpedal=0.5;
	var detune, sig1, sig2, env, sound, fglide, bendSustain;
	bendSustain = sustainpedal;
	fglide = EnvGen.ar(Env.perc(0.05, 0.3, 1, \lin), timeScale:bendSustain) * (-1*slide/4) + 1;
	freq = freq * fglide * Line.kr(1, 1+accelerate, bendSustain);
	detune = 1.005;
	sig1 = MoogFF.ar(
		Pulse.ar(freq, LFNoise1.ar(1.0,0.3,0.4)),
		freq*pitch1, 3
	).dup;
	sig2 = MoogFF.ar(
		Pulse.ar(freq, LFNoise1.ar(1.0,0.2,0.4)),
		freq*pitch1*1.5, 3
	).dup;
	sound = (sig1 + sig2) * 1;
	env = EnvGen.ar(Env.linen(0.001, 0, 0.6, 1, -5), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef(\zure, {|out, sustain=1, pan, accelerate, freq, decay=0, slide=1, detune=1 |
	// var env = EnvGen.ar(Env.linen(0.0001, 0, 0.6, 1, -3), timeScale:sustain, doneAction:2);
	var env = EnvGen.ar(Env.linen(0, 0, 0.6, 1, -2), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	var sound = VarSaw.ar(basefreq, 0, Line.ar(0,0.5,sustain*20/slide));
	var sound2 = VarSaw.ar(basefreq*(detune/100+1), 0, Line.ar(0,0.5,sustain*20/slide));
	var sound3 = VarSaw.ar(basefreq*(-1*detune/100+1), 0, Line.ar(0,0.5,sustain*20/slide));
	var lpfEnv = 400 + EnvGen.ar(Env.linen(0, 0, 0.5, 2000, -30), timeScale:sustain);
	sound = sound - DelayN.ar(sound2,0.2, Line.ar(0,1,5*sustain/slide)/basefreq)
	+ DelayN.ar(sound3,0.2, Line.ar(0,1,20*sustain/slide)/basefreq);
	sound = LPF.ar(sound, lpfEnv);
	Out.ar(out, DirtPan.ar(sound/2, ~dirt.numChannels, pan, env));
}).add;

SynthDef(\insects, {arg out, pan=0, freq=110, sustain=1;
	var modulator, mod1, mod2, mod3, env, f;
	f = 1.43 * (freq / 1108.731);
	modulator = LFSaw.ar(f, 1, 0.5, 0.5);
	mod2 = (modulator * 40.6 * 2pi).cos.squared;
	mod3 = modulator * 3147;
	mod3 = (mod3 * 2pi).cos + ((mod3 * 2 * 2pi).cos * 0.3);
	mod1 = ((Wrap.ar(modulator.min(0.1714) * 5.84) - 0.5).squared * (-4) + 1) * (mod2 * mod3);
	mod1 = (mod1 * 0.1)!2;
	env = EnvGen.ar(Env.linen(0, 0, 1, 1, 10), timeScale: 1/f, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(mod1, ~dirt.numChannels, pan, env));
}).add;

SynthDef("higurashi", {
	arg out, sustain=1, decay=0, pan=0, accelerate=0, slide=0;
	var freq, fglide, detune, sound, sig1, env, parFreq, freqDiffRate, lpf, chain, vibratoFreq=8, vibratoDepth=0.4, real=0.4, imag=0;
	detune = 1.0;
	freq = 4186.009;
	freq = freq - (456.699*slide);
	freq = freq *(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50)));
	freqDiffRate = 0.05;
	freq = freq*(1-freqDiffRate) + EnvGen.ar(Env.linen(0.07, 0.3, 0.5, freq*freqDiffRate, -2), timeScale:sustain);
	lpf = freq+Rand(1000, 3000);
	freq = freq*(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50)));
	sig1 = Saw.ar([freq,freq]);
	sig1 = sig1+0.5*Saw.ar([(freq*2),(freq*2)]);
	sig1 = sig1+0.2*Saw.ar([(freq/2),(freq/2)]);
	sig1 = sig1 + (PinkNoise.ar(1!2)/20);
	sig1 = LPF.ar(sig1, lpf);
	sound = sig1 / 2;
	parFreq = 1 + EnvGen.ar(Env.linen(0, 0, 0.6, 5, -2), timeScale:sustain);
	sound = sound*LFPar.kr(parFreq); // tremoro
	// spectral-conformer
	chain = sound.asArray.collect { |x| FFT(LocalBuf(2048), x) };
	sound = IFFT(
				PV_ConformalMap(chain, real.linlin(0.0,1.0,0.01,2.0), imag.linlin(0.00,1.0,0.01,10.0))
			).tanh;

	env = EnvGen.ar(Env.linen(0, 0, 0.6, 1, -2), timeScale:sustain, doneAction:2);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

SynthDef(\branch, {|out, rate=1, sustain=1, pan, n, delay=1, pitch1=1 |
	var env, sound;
	var spr = 0.005 * delay;
	env = EnvGen.ar(Env.linen(0, 0, 0.2, 1, -10), timeScale:Rand(0.03,0.11), doneAction:2);
	sound = BPF.ar(LPF.ar(WhiteNoise.ar(1), 7500*pitch1), 1500*pitch1);
	sound = Mix.arFill(4, {arg i; sound * 0.5 * EnvGen.ar(Env.new([0,0,1,0],[spr*(i**(0.clip(0,5)+1)),0,0.04/rate]))});
	sound=sound+((Rand(0.4,0.8))*DelayN.ar(sound, sustain, sustain*Rand(0.05,0.2)));
	sound=LPF.ar(sound, Rand(1000,4000));
	sound=sound+(0.1*FreeVerb.ar(sound, 1, 0.8, 0.2));
	sound=sound*10;
	Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env))
}).add;

SynthDef("rustling", {
	arg out, sustain=1, pan;
	var sig, grazing, pebble, branch, footsteps, env, pitchShift;
	env = EnvGen.ar(Env.linen(0.2, 0, 0.25, 1, -5), timeScale:sustain, doneAction:2);

	pitchShift=[0.4,0.5,0.6,1.0].choose;
	grazing=PinkNoise.ar(1);
	grazing=if(
		pitchShift==1.0,
		grazing*0.85,
		PitchShift.ar(grazing, 0.1, pitchShift, 0, 0);
	);
	grazing=LPF.ar(grazing,2000);
	grazing=HPF.ar(grazing,8000);
	grazing=grazing*3;

	pebble=LPF.ar(GrayNoise.ar(1), 1000);
	pebble=LPF.ar(pebble,3000);
	pebble=HPF.ar(pebble,4000);
	pebble=pebble*4;
	pebble=pebble*(EnvGen.ar(Env.linen(sustain*0.6, 1, 0.1, 1)));

	branch=BPF.ar(LPF.ar(WhiteNoise.ar(100), 7500), 1500);
	branch = Mix.arFill(4, {arg i; branch * 0.5 * EnvGen.ar(Env.new([0,0,1,0],[0.005*(i**(0.clip(0,5)+1)),0,0.04]))});
	branch=branch*(EnvGen.ar(Env.linen(0, 0, 0.2, 1, -10), timeScale:Rand(0.06,0.11)));
	branch=DelayN.ar(branch, sustain, sustain*Rand(0.15,0.5));
	branch=branch+((Rand(0.4,0.8))*DelayN.ar(branch, sustain, sustain*Rand(0.1,0.7)));
	branch=LPF.ar(branch, Rand(3000,5000));
	branch=branch+(0.1*FreeVerb.ar(branch, 1, 0.8, 0.2));
	branch=branch*0.017;

	footsteps=SinOsc.ar([65.406,69.296]).dup;
	footsteps=HPF.ar(footsteps, 300);
	footsteps=footsteps*(EnvGen.ar(Env.linen(0, 0, 0.2, 1, -5), timeScale:Rand(0.1,0.2)));
	footsteps=DelayN.ar(footsteps, sustain, sustain*0.1);
	footsteps=footsteps*0.29;

	sig=(grazing*(Rand(0.0,1.0)))+(pebble*Rand(0.5,1.0))+(branch*(Rand(0.0,1.0)**2))+(footsteps*Rand(0.5,1.0));
	sig=sig*10;

	OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env));
}).add;

SynthDef("frog", { arg out, pan=0, amp=0.1, variation=0.9;
	var dt, n, freq, mul, t, u;
	n = Rand(7, 35);
	dt = 25.0 + Rand(-1.7, 1.7);
	dt = dt + LFNoise2.kr(2, variation) * 0.001;
	freq = 901 + Rand(0, 65);
	t = Impulse.ar(dt.reciprocal, 0, 100);
	mul = PulseCount.ar(t) < n;
	u = BPF.ar(mul * t, freq, 0.1);
	u = BPF.ar(u, freq, 0.2);
	u = u * 5;
	DetectSilence.ar(u, doneAction:2);
	Out.ar(out, Pan2.ar(u, pan, amp));
}).add;

// ハシブトガラス
SynthDef(\jungleCrow, { arg out, pan=0, amp=0.1, variation=0.9, sustain=1, vibratoDepth=0;
	var dt, n, freq, mul, t, u, chain, env, vibratoFreq=70, freqDiffRate=0.1, real=0.5, imag=0;
	env = EnvGen.ar(Env.linen(0.04, 0, 1, 1, 8), timeScale: sustain, doneAction:2);
	freq = 960 + Rand(0, 10);
	freq = freq *(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50)));
	freq = freq*(1-freqDiffRate) + EnvGen.ar(Env.linen(0, 0, 1, freq*freqDiffRate, 7), timeScale:sustain);
	u = 0;
	u = u + (SinOsc.ar(freq*2) * 0.15);
	u = u + (SinOsc.ar(freq*1.95) * 0.07);
	u = u + (SinOsc.ar(freq*1.5) * 0.13);
	u = u + (SinOsc.ar(freq*1.45) * 0.09);
	u = u + (SinOsc.ar(freq*1.04) * 0.07);
	u = u + (SinOsc.ar(freq*1) * 0.09);
	OffsetOut.ar(out, DirtPan.ar(u, ~dirt.numChannels, pan, env));
}).add;

// ハシボソガラス
SynthDef(\carrionCrow, { arg out, pan=0, amp=0.1, variation=0.9, sustain=1;
	var dt, n, freq, mul, t, u, chain, env, vibratoFreq=50, vibratoDepth=2, freqDiffRate=0.25, real=0.5, imag=0;
	env = EnvGen.ar(Env.linen(0.0, 0, 1, 1, 5), timeScale: sustain, doneAction:2);
	// n = Rand(40, 46);
	n = 45;
	// dt = 25.0 + Rand(-1.7, 1.7);
	dt = 25;
	dt = dt + LFNoise2.kr(2, variation) * 0.0006;
	// freq = 1244.508 + Rand(0, 323.474);
	freq = 1244.508;
	freq = freq*(1-freqDiffRate) + EnvGen.ar(Env.linen(0, 0, 1, freq*freqDiffRate, 5), timeScale:sustain);
	t = Impulse.ar(dt.reciprocal, 0, 100);
	mul = PulseCount.ar(t) < n;
	u = BPF.ar(mul * t, freq, 0.1);
	u = BPF.ar(u, freq, 0.4);
	u = HPF.ar(u, 6000);
	u = LPF.ar(u, 2000);
	// spectral-conformer
	chain = u.asArray.collect { |x| FFT(LocalBuf(2048), x) };
	2.do{
		u = IFFT(
				PV_ConformalMap(chain, real.linlin(0.0,1.0,0.01,2.0), imag.linlin(0.00,1.0,0.01,10.0))
			).tanh;
	};
	u = u*10;
	OffsetOut.ar(out, DirtPan.ar(u, ~dirt.numChannels, pan, env));
}).add;

// キジバト
SynthDef(\turtledove, { arg out, pan=0, sustain=1, freq=410;
	var dt, n, mul, t, u, chain, env, freqDiffRate=0.1, real=0.5, imag=0;
	env = EnvGen.ar(Env.linen(0.04, 0, 1, 1, 5), timeScale: sustain, doneAction:2);
	freq = freq*(1-freqDiffRate) + EnvGen.ar(Env.linen(0, 0, 1, freq*freqDiffRate, 5), timeScale:sustain);
	u = 0;
	u = u + (SinOsc.ar(freq*1.04) * 0.07);
	u = u + (SinOsc.ar(freq*1) * 0.09);
	OffsetOut.ar(out, DirtPan.ar(u, ~dirt.numChannels, pan, env));
}).add;
)
