(
SynthDef("background", {
	arg amp=1;
	var out=0;
	p=PinkNoise.ar(1!2);
	p=HPF.ar(p, 50);
	p=LPF.ar(p, 1000);
	Out.ar(out, p*0.05*amp);
}, [1]).add;
)

b = Synth("background", [\amp, 0]);
b.set(\amp, 2);



(
SynthDef("sine", {
	arg freq = 440, amp = 0.2, parFreq=0, pan2Freq=0, pan=0,
	vibratoFreq=3, vibratoDepth=0, actave=1, reverb=0, ice=0;
	var sig, out=0, chain, in, z, y, oct, left, right;
	sig = SinOsc.ar(actave*freq*(1+((LFPar.ar(vibratoFreq)+1)*(vibratoDepth/50))), 0, amp);
	// tremoro
	sig = if(
		parFreq>0.1,
		sig*LFPar.ar(parFreq),
		if(
			LFPar.ar(parFreq)>0,
			sig*(LFPar.ar(parFreq) + (1-(LFPar.ar(parFreq))*((1-(parFreq*10))))),
			sig*(LFPar.ar(parFreq) + (-1-(LFPar.ar(parFreq))*((1-(parFreq*10))))),
		)
	);
	// panning
	sig = if(
		pan2Freq>0.1,
		Pan2.ar(sig, LFPar.ar(pan2Freq)),
		Pan2.ar(sig, LFPar.ar(pan2Freq)*pan2Freq*10)
	);
	// fixed panning
	right=sig;
	left=sig;
	sig = if(
		pan2Freq==0,
		sig=NumChannels.ar([left*(1-pan)/2, right*(1+pan)/2], 2),
		sig
	);
	// reverb
	z = DelayN.ar(sig, 0.048);
	y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, 1, 15) }));
	32.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
	oct = 1.0 * LeakDC.ar( abs(y) );
	y = SelectX.ar(ice, [y, ice * oct, DC.ar(0)]);
	sig = sig + (0.2*y*reverb);
    Out.ar(out, sig);
}, [0.3, 2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 1]).add;

SynthDef("sunsetForest", {
	arg amp=1, groundNoise=0;
	var a, out=0;
	a=PinkNoise.ar(1!2);
	75.do{
		a=BBandStop.ar(a,LFNoise1.kr(0.01.rand).range(10,2500),Rand(0.9,1))
	};
	a=LPF.ar(a,7800);
	a=HPF.ar(a,1500-(groundNoise*1400));
	Out.ar(out, amp * a * 5);
}, [30, 1]).add;

SynthDef(\rain, {
	arg amp=1;
    var sig, out=0;
    sig = { TRand.ar(-1, 1, Dust.ar(25000)) } ! 2;
	sig = BPF.ar(sig, 100, 0.5);
	sig = HPF.ar(sig, 6000);
	sig = LPF.ar(sig, 5000);
	sig = sig*10*amp;
	Out.ar(out, sig);
}, [30]).add;
)

s = Synth("sunsetForest", [\amp, 0, \groundNoise, 0]);
s.set(\amp, 0.5);
s.set(\amp, 0);
s.set(\groundNoise, 0);
s.free;

r = Synth("rain", [\amp, 0]);
r.set(\amp, 1);
r.set(\amp, 0);
r.free;

a = Synth("sine", [\freq, 570.609/4, \amp, 0, \parFreq, 1.5, \pan2Freq, 0, \pan, 0.7, \vibratoDepth, 0.3, \vibratoFreq, 3, \actave, 1, \reverb, 0.6]);
a.set(\amp, 0.05);

(
a = Synth("sine", [\freq, 570.609/4, \amp, 0, \parFreq, 1, \pan2Freq, 1, \vibratoDepth, 0.3, \vibratoFreq, 3, \actave, 1, \reverb, 0.6]);
b = Synth("sine", [\freq, 640.487/4, \amp, 0, \parFreq, 0, \pan2Freq, 1, \vibratoDepth, 0.3, \vibratoFreq, 3, \actave, 1, \reverb, 0.4]);
c = Synth("sine", [\freq, 659.255/8, \amp, 0, \parFreq, 0, \pan2Freq, 1, \vibratoDepth, 0.3, \vibratoFreq, 3, \actave, 1, \reverb, 0.9]);
)
(
a.set(\amp, 0.03);
b.set(\amp, 0.04);
c.set(\amp, 0.05);
)
(
a.set(\pan2Freq, 80);
b.set(\pan2Freq, 100);
c.set(\pan2Freq, 80);
)
(
a.set(\ice, 0.3);
b.set(\ice, 0.2);
c.set(\ice, 0.2);
)
(
a.set(\amp, 0);
b.set(\amp, 0);
c.set(\amp, 0);
)
(
a.free;
b.free;
c.free;
)


Ndef(\fire).play;
Ndef(\fire).fadeTime = 10;
Ndef(\fire).stop;
Ndef(\fire).clear;
(
Ndef(\fire,{|amp = 1.75, freq = 1, lpf = 6000, hpf = 100|
	x=DelayL.ar(n=Ndef(\fire);n.ar,2,LFNoise0.kr(3.5*_!4)+1)+Blip.ar(freq);
	amp*LeakDC.ar(HPF.ar(LPF.ar(x+x.mean*0.15,lpf), hpf)).sin;
});
)
Ndef(\fire).set(\amp, 1.75, \freq, 1, \lpf, 6000, \hpf, 100);
Ndef(\fire).set(\amp, 1, \freq, 0.55, \lpf, 4000, \hpf, 50);
Ndef(\fire).set(\amp, 0, \freq, 0.6, \lpf, 4000, \hpf, 100);


