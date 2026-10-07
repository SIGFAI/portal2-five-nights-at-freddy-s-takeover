// Demo: one night shift in about 75 s. Everything shown is the mod doing its job.
::fzNearest <- function() {
	local best = null
	local bd = 99999.0
	local pp = SigfHost().GetOrigin()
	foreach (A in ::fz.anims) {
		if (!A.alive || A.ent == null || !A.ent.IsValid()) continue
		local d = ::fzDist(A.pos, pp)
		if (d < bd) { bd = d; best = A }
	}
	return best
}

::fzEyeFwd <- function() { return ::fz.eyes.GetForwardVector() }
::fzTarget <- null

// A cube thrown from the player's hands at the chosen animatronic.
::fzThrowCube <- function(kind) {
	local t = ::fzTarget
	if (t == null || !t.alive || t.ent == null || !t.ent.IsValid()) t = ::fzNearest()
	if (t == null) return
	::fzTarget = t
	SigfLookAt(t.pos + Vector(0, 0, 45))
	SigfIn(0.35, function():(t, kind) {
		local f = ::fzEyeFwd()
		SigfCube(SigfHost().EyePosition() + f * 55.0 + Vector(0, 0, -12), kind, 14.0, function(c):(t) {
			local d = (t.pos + Vector(0, 0, 45)) - c.GetOrigin()
			d.Norm()
			SigfPush(c, d * 700.0 + Vector(0, 0, 90))
		})
	})
}

SigfDemo(0.2, function() { SigfLook(8, 0) })
SigfDemo(0.6, function() { ::fzRestartNight(); ::fz.noCatch = true })
SigfDemo(0.8, function() { SigfSound("sigf/phone.wav") })
SigfDemo(3.8, function() { SigfSound("sigf/phoneguy.wav") })
SigfDemo(1.5, function() { SigfCaption("FIVE NIGHTS AT APERTURE", 3.5) })
SigfDemo(3.0, function() {
	local f = ::fzFirst("FREDDY")
	if (f != null) SigfLookAt(f.pos + Vector(0, 0, 50))
})
SigfDemo(5.5, function() { SigfCaption("SURVIVE 12 AM TO 6 AM", 3.5) })
SigfDemo(9.5, function() {
	SigfCaption("LOOK AWAY... THEY CREEP CLOSER", 5.0)
	SigfLook(8, 150)
})
SigfDemo(16.0, function() {
	SigfCaption("STARE AT THEM: THEY FREEZE", 4.5)
	local a = ::fzNearest()
	if (a != null) SigfLookAt(a.pos + Vector(0, 0, 50))
})
SigfDemo(21.0, function() {
	SigfCaption("THROW CUBES AT THEM", 5.0)
	::fzTarget = null
	::fzThrowCube(1)
})
SigfDemo(23.2, function() { ::fzThrowCube(0) })
SigfDemo(25.4, function() { ::fzThrowCube(1) })
SigfDemo(27.6, function() { ::fzThrowCube(0) })
SigfDemo(31.5, function() {
	// the one the player is facing goes through a portal and comes out behind
	local best = null
	local bd = -2.0
	foreach (A in ::fz.anims) {
		if (!A.alive || A.ent == null || !A.ent.IsValid()) continue
		local v = ::fzViewDot(A.pos)
		if (v > bd) { bd = v; best = A }
	}
	if (best == null) return
	SigfCaption("PORTALS SEND THEM FLYING", 4.0)
	SigfLookAt(best.pos + Vector(0, 0, 45))
	local a = Vector(best.pos.x, best.pos.y, ::fz.floorZ)
	local b = ::fzPickSpot(::fzLookYaw() + (RandomInt(0, 1) == 0 ? 50.0 : -50.0), 150.0)
	local tries = 0
	while (::fzDist(a, b) < 110.0 && tries < 6) { b = ::fzPickSpot(::fzLookYaw() + RandomFloat(-70, 70), 130.0); tries++ }
	::fzPortalPair(a, b, 9.0)
})
SigfDemo(40.0, function() {
	SigfCaption("DON'T LET THEM REACH YOU", 3.5)
	local c = ::fzFirst("CHICA")
	if (c == null) c = ::fzNearest()
	if (c == null) return
	local pp = SigfHost().GetOrigin()
	local np = Vector(pp.x - 105.0, pp.y, ::fz.floorZ)
	if (!::fzStandable(np)) np = Vector(pp.x, pp.y + 105.0, ::fz.floorZ)
	::fzMove(c, np)
	c.next = Time() + 1.0
	c.stun = 0.0
	::fz.noCatch = false
	SigfLook(8, 0)
})
SigfDemo(47.0, function() {
	SigfCaption("THE BATTERY IS DYING...", 3.5)
	::fz.power = 3.5
	local a = ::fzNearest()
	if (a != null) SigfLookAt(a.pos + Vector(0, 0, 50))
})
SigfDemo(55.0, function() { SigfCaption("NO LIGHT: THEY MOVE FASTER", 3.5) })
SigfDemo(66.0, function() {
	SigfCaption("THEY GET FASTER EVERY NIGHT", 4.0)
	local a = ::fzNearest()
	if (a != null) SigfLookAt(a.pos + Vector(0, 0, 50))
})
SigfDemo(71.0, function() {
	local a = ::fzNearest()
	if (a != null) SigfLookAt(a.pos + Vector(0, 0, 50))
})
