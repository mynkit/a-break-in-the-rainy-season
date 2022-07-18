(
SynthDef("background", {
	arg amp=1;
	var out=0;
	p=PinkNoise.ar(1!2);
	p=HPF.ar(p, 50);
	p=LPF.ar(p, 1000);
	Out.ar(out, p*0.05*amp);
}, [1]).add;
SynthDef("leavesOfTreesEndless", {
	arg amp=1, pan=0, sustain=1;
	var out=0, sig, env;
	sig=PinkNoise.ar(1!2);
	sig=HPF.ar(sig, 1000);
	sig=LPF.ar(sig, 10000);
	pan = LFPar.ar(0.1).range(0, 0.9);
	amp = LFPar.ar(0.03).range(amp*0.5, amp);
	Out.ar(out, DirtPan.ar(sig*0.1*amp, ~dirt.numChannels, pan));
}, [30]).add;
)

b = Synth("background", [\amp, 0]);
b.set(\amp, 1.5);
b.free;

l = Synth("leavesOfTreesEndless", [\amp, 0]);
l.set(\amp, 0.16);
l.free;

