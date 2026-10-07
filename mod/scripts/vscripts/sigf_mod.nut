// Five Nights at Freddy's Takeover: the dark chamber becomes a night shift.
// Four animatronics (built on Portal turrets) creep closer whenever you look away.
// Look at them with the flashlight to freeze and scare them off, hit them with cubes, send them through portals.

::fz <- {
	night = 1, nightStart = 0.0, hour = -1, power = 100.0, powerOut = false,
	floorZ = 0.0, center = Vector(0, 0, 0), spots = [], eyes = null, ready = false,
	freezeUntil = 0.0, scrapped = 0, caught = 0, anims = [], cubeSeen = {}, overlayUntil = 0.0, hpText = "",
	noCatch = false, portals = [], lastHud = 0.0, ovSeq = 0, lastDbg = 0.0, lastOv = 0.0, musicAt = 0.0, intro = false
}
::FZ_HOUR <- 9.5

// ---------- the cast ----------
// parts: model, offset (forward, left, up) in turret space, scale, color
::FZ_SPH <- "models/npcs/personality_sphere/personality_sphere.mdl"
::FZ_CAST <- [
	{ name = "FREDDY", color = "100 55 22", scare = "sigf/scare_freddy", hp = 4, interval = 2.9, step = 42, wake = 0, scale = 1.5,
	  parts = [
		{ m = ::FZ_SPH, o = Vector(0, 20, 126), s = 0.85, c = "135 78 36" },
		{ m = ::FZ_SPH, o = Vector(0, -20, 126), s = 0.85, c = "135 78 36" },
		{ m = ::FZ_SPH, o = Vector(0, 0, 142), s = 0.8, c = "12 12 12" },
		{ m = ::FZ_SPH, o = Vector(0, 0, 160), s = 0.55, c = "12 12 12" },
		{ m = ::FZ_SPH, o = Vector(26, 0, 62), s = 0.4, c = "200 20 20" },
		{ m = ::FZ_SPH, o = Vector(24, 0, 78), s = 0.5, c = "200 150 95" }
	  ] },
	{ name = "BONNIE", color = "95 50 140", scare = "sigf/scare_bonnie", hp = 3, interval = 2.6, step = 44, wake = 0, scale = 1.5,
	  parts = [
		{ m = ::FZ_SPH, o = Vector(0, 11, 138), s = 0.6, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(0, 12, 156), s = 0.5, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(0, 13, 172), s = 0.45, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(0, -11, 138), s = 0.6, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(0, -12, 156), s = 0.5, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(0, -13, 172), s = 0.45, c = "125 72 175" },
		{ m = ::FZ_SPH, o = Vector(26, 0, 62), s = 0.4, c = "200 20 20" }
	  ] },
	{ name = "CHICA", color = "190 150 20", scare = "sigf/scare_chica", hp = 3, interval = 2.8, step = 42, wake = 1, scale = 1.4,
	  parts = [
		{ m = ::FZ_SPH, o = Vector(24, 0, 54), s = 0.85, c = "250 250 255" },
		{ m = ::FZ_SPH, o = Vector(30, 0, 76), s = 0.45, c = "255 120 0" },
		{ m = ::FZ_SPH, o = Vector(0, 0, 138), s = 0.5, c = "190 150 20" },
		{ m = ::FZ_SPH, o = Vector(0, 11, 146), s = 0.4, c = "190 150 20" },
		{ m = ::FZ_SPH, o = Vector(0, -11, 146), s = 0.4, c = "190 150 20" }
	  ] },
	{ name = "FOXY", color = "150 28 18", scare = "sigf/scare_foxy", hp = 3, interval = 2.0, step = 52, wake = 2, scale = 1.45,
	  parts = [
		{ m = ::FZ_SPH, o = Vector(0, 18, 130), s = 0.7, c = "140 26 16" },
		{ m = ::FZ_SPH, o = Vector(0, -18, 130), s = 0.7, c = "140 26 16" },
		{ m = ::FZ_SPH, o = Vector(26, -8, 96), s = 0.35, c = "8 8 8" },
		{ m = ::FZ_SPH, o = Vector(18, 30, 40), s = 0.4, c = "190 190 190" }
	  ] }
]

// ---------- geometry helpers ----------
::fzYawOf <- function(v) { return atan2(v.y, v.x) * 57.29578 }
::fzRot <- function(x, y, yaw) { local r = yaw / 57.29578; return Vector(x * cos(r) - y * sin(r), x * sin(r) + y * cos(r), 0) }
::fzFlat <- function(v) { return Vector(v.x, v.y, 0) }
::fzDist <- function(a, b) { return ::fzFlat(a - b).Length() }

::fzStandable <- function(p) {
	local t = Vector(p.x, p.y, ::fz.floorZ + 30)
	local f = TraceLine(t, t - Vector(0, 0, 120), null)
	return f < 1.0 && (t.z - 120.0 * f) > ::fz.floorZ - 12
}

::fzClear <- function(a, b) {
	local f = TraceLine(a + Vector(0, 0, 36), b + Vector(0, 0, 36), null)
	return f > 0.995
}

::fzSpotsInit <- function() {
	local c = ::fz.center
	local out = []
	foreach (r in [110.0, 150.0, 190.0, 230.0]) {
		for (local a = 0; a < 360; a += 15) {
			local o = ::fzRot(r, 0, a)
			local p = Vector(c.x + o.x, c.y + o.y, ::fz.floorZ)
			local solid = ::fzStandable(p) && ::fzClear(c, p)
			foreach (m in [Vector(34, 0, 0), Vector(-34, 0, 0), Vector(0, 34, 0), Vector(0, -34, 0)]) { if (solid && !::fzStandable(p + m)) solid = false }
			if (solid) out.append({ p = p, a = a, r = r })
		}
	}
	::fz.spots = out
}

// Standable spot as far as possible from the player near a wanted yaw.
::fzPickSpot <- function(yaw, minR) {
	local best = null
	local bestScore = -99999.0
	foreach (s in ::fz.spots) {
		if (s.r < minR) continue
		local d = fabs(s.a - yaw) % 360.0
		d = d > 180 ? 360 - d : d
		local score = s.r * 0.5 - d
		foreach (A in ::fz.anims) { if (A.alive && ::fzDist(A.pos, s.p) < 60) score -= 100 }
		if (score > bestScore) { bestScore = score; best = s.p }
	}
	if (best == null) best = ::fz.center + Vector(150, 0, 0)
	return best
}

::fzLookYaw <- function() {
	local e = ::fz.eyes
	if (e == null || !e.IsValid()) return 0.0
	return ::fzYawOf(e.GetForwardVector())
}

// 1.0 = the animatronic is dead center on screen, < 0 = behind the player.
::fzViewDot <- function(p) {
	local e = ::fz.eyes
	local host = SigfHost()
	if (e == null || !e.IsValid()) return -1.0
	local d = (p + Vector(0, 0, 40)) - host.EyePosition()
	local len = d.Length()
	if (len < 1.0) return 1.0
	local f = e.GetForwardVector()
	return (f.x * d.x + f.y * d.y + f.z * d.z) / len
}

// ---------- screen effects ----------
::fzOverlay <- function(mat) {
	::fz.ovSeq++
	if (mat == null) { SigfCmd("r_screenoverlay \"\""); return }
	SigfCmd("r_screenoverlay " + mat)
}

::fzBaseOverlay <- function() { ::fzOverlay("sigf/vignette") }

::fzShake <- function(amp, sec) {
	SigfSpawn("env_shake", SigfHost().GetOrigin(), { amplitude = amp, radius = 1500, duration = sec, frequency = 40, spawnflags = 29 }, sec + 1.0,
		function(e) { EntFireByHandle(e, "StartShake", "", 0.0, null, null) })
}

::fzStatic <- function(sec) {
	::fzOverlay("sigf/staticfx")
	SigfIn(sec, function() { if (Time() > ::fz.overlayUntil) ::fzBaseOverlay() })
}

// ---------- animatronics ----------
::fzSpawn <- function(def, pos) {
	local A = { def = def, name = def.name, hp = def.hp, alive = false, ent = null, parts = [], pos = pos,
		next = Time() + def.interval, respawn = false, stun = 0.0, portalCool = 0.0, lookTime = 0.0, unseen = 0.0, hitCool = 0.0, color = def.color, pending = true }
	::fz.anims.append(A)
	local yaw = ::fzYawOf(::fzFlat(SigfHost().GetOrigin() - pos))
	SigfTurret(pos, yaw, 0.0, function(t):(A, def, pos, yaw) {
		A.ent = t
		A.alive = true
		A.pending = false
		SigfColor(t, def.color)
		SigfScale(t, def.scale)
		foreach (p in def.parts) ::fzPart(A, p, pos, yaw)
		SigfIn(0.3, function():(A, pos) { if (A.alive && A.ent != null && A.ent.IsValid()) { A.ent.SetOrigin(pos); A.pos = pos } })
	})
	return A
}

// Parts (ears, hat...) are made far above the turret and without collision (a solid part would knock the turret over),
// then moved onto it and parented: they follow every move and turn of the turret.
::fzPart <- function(A, p, pos, yaw) {
	local root = A.ent
	SigfDynamic(p.m, pos + Vector(0, 0, 600), 0.0, function(e):(A, p, root) {
		EntFireByHandle(e, "DisableCollision", "", 0.0, null, null)
		SigfScale(e, p.s)
		SigfColor(e, p.c)
		A.parts.append({ e = e, o = p.o })
		SigfIn(0.25, function():(A, p, e, root) {
			if (!e.IsValid() || root == null || !root.IsValid()) return
			local yy = ::fzYawOf(::fzFlat(SigfHost().GetOrigin() - A.pos))
			local o = ::fzRot(p.o.x, p.o.y, yy)
			e.SetOrigin(Vector(A.pos.x + o.x, A.pos.y + o.y, ::fz.floorZ + p.o.z))
			e.SetAngles(0, yy, 0)
			EntFireByHandle(e, "SetParent", "!activator", 0.1, root, null)
		})
	})
}

::fzMove <- function(A, newPos) {
	A.pos = newPos
	if (A.ent != null && A.ent.IsValid()) {
		local yaw = ::fzYawOf(::fzFlat(SigfHost().GetOrigin() - newPos))
		A.ent.SetOrigin(newPos)
		A.ent.SetAngles(0, yaw, 0)
	}
}

::fzFlash <- function(A, rgb, sec) {
	if (A.ent == null || !A.ent.IsValid()) return
	SigfColor(A.ent, rgb)
	SigfIn(sec, function():(A) { if (A.alive && A.ent != null && A.ent.IsValid()) SigfColor(A.ent, A.color) })
}

::fzStep <- function(A, towards, dist) {
	local from = A.pos
	local dir = ::fzFlat(towards - from)
	local len = dir.Length()
	if (len < 1.0) return false
	if (::fz.noCatch && len < 105.0 && dist > 0) return false
	local yaw = ::fzYawOf(dir)
	foreach (j in [0, 25, -25, 50, -50]) {
		local o = ::fzRot(dist, 0, yaw + j)
		local np = Vector(from.x + o.x, from.y + o.y, ::fz.floorZ)
		if (::fzStandable(np) && ::fzClear(from, np)) { ::fzMove(A, np); return true }
	}
	return false
}

::fzHud <- function(now) {
	if (now < ::fz.lastHud + 1.0) return
	::fz.lastHud = now
	local h = ::fz.hour < 0 ? 0 : ::fz.hour
	local label = h == 0 ? "12 AM" : (h.tostring() + " AM")
	SigfText(label, -1.0, 0.03, 1.3, "255 255 255", 3)
	SigfText("Night " + ::fz.night + "     Scrapped " + ::fz.scrapped, -1.0, 0.11, 1.3, "255 200 80", 0)
	local pw = ::fz.power.tointeger()
	SigfText("Power: " + pw + "%", 0.04, 0.88, 1.3, pw > 25 ? "255 255 255" : "255 60 60", 4)
	local bars = ""
	for (local i = 0; i < 12; i++) bars += (i * 100 / 12 < pw) ? "|" : "."
	SigfText(bars, 0.04, 0.94, 1.3, pw > 25 ? "120 255 120" : "255 60 60", 2)
}

::fzCaught <- function(A) {
	local now = Time()
	::fz.freezeUntil = now + 2.4
	::fz.overlayUntil = now + 2.4
	::fz.caught++
	::fzOverlay(A.def.scare)
	SigfSound("sigf/scream.wav")
	::fzShake(18.0, 1.6)
	SigfText(A.name + " GOT YOU!", -1.0, 0.84, 2.2, "255 40 40", 5)
	SigfIn(2.4, function():(A) {
		::fzBaseOverlay()
		// everyone backs off: a short breather after a scare
		foreach (B in ::fz.anims) {
			if (!B.alive) continue
			::fzMove(B, ::fzPickSpot(::fzLookYaw() + RandomFloat(-110, 110), 200.0))
			B.next = Time() + B.def.interval * 2.2
			B.lookTime = 0.0
		}
	})
}

::fzHit <- function(A, dir) {
	local now = Time()
	A.hitCool = now + 0.6
	A.hp--
	A.stun = now + 2.5
	::fzFlash(A, "255 0 0", 0.3)
	SigfSound("sigf/clang.wav")
	SigfIn(0.15, function() { SigfSound("sigf/pain.wav") })
	SigfParticle("cable_sparks_b", A.pos + Vector(0, 0, 45), 2.0)
	SigfParticle("bot_death_B_flash_spark", A.pos + Vector(0, 0, 45), 2.0)
	local k = Vector(dir.x, dir.y, 0)
	if (k.Length() > 0.1) {
		k.Norm()
		local np = A.pos + k * 60.0
		if (::fzStandable(np)) ::fzMove(A, np)
	}
	local bars = ""
	for (local i = 0; i < A.def.hp; i++) bars += (i < A.hp) ? "#" : "-"
	SigfText(A.name + "  [" + bars + "]", -1.0, 0.72, 1.6, "255 90 90", 1)
	if (A.hp <= 0) ::fzScrap(A)
}

::fzScrap <- function(A) {
	A.alive = false
	::fz.scrapped++
	::fzFlash(A, "255 255 255", 0.1)
	if (A.ent != null && A.ent.IsValid()) EntFireByHandle(A.ent, "SelfDestruct", "", 0.2, null, null)
	SigfCaption(A.name + " IS OUT OF ORDER!", 2.5)
	SigfSound("sigf/staticburst.wav")
	::fzStatic(0.5)
	// A repaired copy comes back later, a bit tougher.
	local def = A.def
	A.respawn = true
	SigfIn(11.0, function():(def, A) {
		A.respawn = false
		local yaw = ::fzLookYaw() + RandomFloat(-120, 120)
		local nA = ::fzSpawn(def, ::fzPickSpot(yaw, 230.0))
		SigfSound("sigf/wake.wav")
		SigfText(def.name + " HAS BEEN REPAIRED", -1.0, 0.72, 2.0, "255 200 80", 1)
	})
}

// Fast cubes hurt animatronics (the hit and the shove are real physics, the damage is ours).
::fzSegDist <- function(a, b, p) {
	local ab = b - a
	local l2 = ab.Dot(ab)
	local t = l2 < 1.0 ? 0.0 : (p - a).Dot(ab) / l2
	t = t < 0.0 ? 0.0 : (t > 1.0 ? 1.0 : t)
	return (a + ab * t - p).Length()
}

::fzCubeHits <- function(now) {
	local seen = {}
	for (local c = Entities.FindByClassname(null, "prop_weighted_cube"); c != null; c = Entities.FindByClassname(c, "prop_weighted_cube")) {
		local id = c.entindex()
		local p = c.GetOrigin()
		local prev = p
		if (id in ::fz.cubeSeen) prev = ::fz.cubeSeen[id].p
		local vel = (p - prev) * 10.0
		seen[id] <- { p = p }
		if (vel.Length() < 140.0) continue
		foreach (A in ::fz.anims) {
			if (!A.alive || A.hitCool > now) continue
			if (::fzSegDist(prev, p, A.pos + Vector(0, 0, 40)) < 58.0) { ::fzHit(A, vel); break }
		}
	}
	::fz.cubeSeen = seen
}

::fzTick <- function() {
	local now = Time()
	local host = SigfHost()
	if (host == null || !::fz.ready) return
	::fzHud(now)
	if (now > ::fz.overlayUntil && now > ::fz.lastOv + 3.0) { ::fz.lastOv = now; ::fzBaseOverlay() }

	// the clock
	local hour = ((now - ::fz.nightStart) / ::FZ_HOUR).tointeger()
	if (hour != ::fz.hour) {
		::fz.hour = hour
		if (hour >= 6) { ::fzDawn(); return }
		::fzWakeUp(hour)
	}
	// the battery drains while the flashlight is on
	if (!::fz.powerOut) {
		::fz.power -= 0.1 * 0.45 * (1.0 + (::fz.night - 1) * 0.2)
		if (::fz.power <= 0.0) {
			::fz.power = 0.0
			::fz.powerOut = true
			SigfCmd("impulse 100")
			SigfSound("sigf/flick.wav")
			SigfIn(0.4, function() { SigfSound("sigf/poweroff.wav") })
			SigfCaption("OUT OF POWER...", 3.0)
			SigfIn(3.5, function() { ::fzMusicBox() })
		}
	}
	if (now < ::fz.freezeUntil) return

	::fzCubeHits(now)
	::fzPortalHop(now)
	local pp = host.GetOrigin()
	foreach (A in ::fz.anims) {
		if (!A.alive || A.pending) continue
		if (A.ent == null || !A.ent.IsValid()) {
			// it fell off the platform or was destroyed: a repaired copy comes back
			A.alive = false
			A.respawn = true
			local def = A.def
			SigfIn(6.0, function():(def, A) { A.respawn = false; ::fzSpawn(def, ::fzPickSpot(::fzLookYaw() + RandomFloat(-110, 110), 200.0)); SigfSound("sigf/wake.wav") })
			continue
		}
		// sync with the real entity (portals and physics can move it)
		local real = A.ent.GetOrigin()
		if (::fzDist(real, A.pos) > 8.0 && !A.pending) {
			local rp = Vector(real.x, real.y, ::fz.floorZ)
			if (::fzStandable(rp) && real.z > ::fz.floorZ - 30) A.pos = rp
			else ::fzMove(A, A.pos)
		}
		local d = ::fzDist(A.pos, pp)
		if (::fz.noCatch && d < 125.0) {
			// demo: keep them at a filmable distance
			local away = ::fzFlat(A.pos - pp)
			if (away.Length() < 1.0) away = Vector(1, 0, 0)
			away.Norm()
			local np = Vector(pp.x + away.x * 140.0, pp.y + away.y * 140.0, ::fz.floorZ)
			if (::fzStandable(np)) { ::fzMove(A, np); d = 140.0 }
		}
		if (d < 72.0 && !::fz.noCatch) { ::fzCaught(A); return }
		local looked = ::fzViewDot(A.pos) > 0.82 && d < 700.0 && !::fz.powerOut && ::fzClear(host.GetOrigin(), A.pos)
		if (looked) {
			A.lookTime += 0.1
			A.unseen = 0.0
			if (A.lookTime > 1.8 && A.stun < now) {
				// scared off by the flashlight
				A.lookTime = 0.0
				A.stun = now + 3.0
				::fzFlash(A, "255 255 255", 0.2)
				SigfSound("sigf/staticburst.wav")
				::fzStep(A, pp - (A.pos - pp), 70.0)
			}
			continue
		}
		A.lookTime = 0.0
		A.unseen += 0.1
		if (A.stun > now || A.next > now) continue
		local mult = 1.0 + (::fz.night - 1) * 0.35 + (::fz.powerOut ? 0.8 : 0.0)
		if (A.name == "FOXY" && A.unseen > 4.0) mult += 1.0
		A.next = now + A.def.interval * RandomFloat(0.75, 1.2) / mult
		if (::fzStep(A, pp, A.def.step) && d < 420.0) SigfSound("sigf/thump.wav")
	}
}

// The music box plays while the power is out; they move faster.
::fzMusicBox <- function() {
	if (!::fz.powerOut) return
	SigfSound("sigf/musicbox.wav")
	SigfIn(12.5, function() { ::fzMusicBox() })
}

// ---------- the night ----------
::fzWakeUp <- function(hour) {
	foreach (def in ::FZ_CAST) {
		if (def.wake != hour) continue
		local dup = false
		foreach (A in ::fz.anims) { if (A.name == def.name && (A.alive || A.pending || A.respawn)) dup = true }
		if (dup) continue
		local yaw = ::fzLookYaw() + (def.name == "FREDDY" ? 0.0 : (def.name == "BONNIE" ? 45.0 : (def.name == "CHICA" ? -45.0 : 120.0)))
		::fzSpawn(def, ::fzPickSpot(yaw, 210.0))
		SigfSound("sigf/wake.wav")
		if (hour > 0) SigfCaption(def.name + " HAS AWAKENED", 2.5)
	}
}

::fzDawn <- function() {
	::fz.hour = 6
	::fz.freezeUntil = Time() + 6.0
	SigfSound("sigf/chime.wav")
	SigfCaption("6 AM  -  YOU SURVIVED NIGHT " + ::fz.night + "!", 4.5)
	::fzOverlay(null)
	foreach (A in ::fz.anims) {
		if (!A.alive || A.ent == null || !A.ent.IsValid()) continue
		::fzMove(A, ::fzPickSpot(::fzLookYaw() + 180.0, 200.0))
	}
	SigfIn(6.0, function() { ::fzNewNight() })
}

::fzNewNight <- function() {
	::fz.night++
	::fz.nightStart = Time()
	::fz.hour = -1
	::fz.power = 100.0
	if (::fz.powerOut) { ::fz.powerOut = false; SigfCmd("impulse 100"); SigfSound("sigf/flick.wav") }
	::fzBaseOverlay()
	SigfCaption("NIGHT " + ::fz.night + "  -  12 AM", 3.0)
	foreach (A in ::fz.anims) { if (A.alive) A.next = Time() + 3.0 }
}

::fzRestartNight <- function() {
	::fz.night = 1
	::fz.scrapped = 0
	::fz.ready = true
	if (::fz.powerOut) { ::fz.powerOut = false; SigfCmd("impulse 100") }
	::fz.nightStart = Time() + 0.5
	::fz.hour = -1
	::fz.power = 100.0
	::fz.freezeUntil = 0.0
	::fzBaseOverlay()
}

::fzFirst <- function(name) {
	foreach (A in ::fz.anims) { if (A.name == name && A.alive) return A }
	return null
}

::fzEyesInit <- function() {
	local host = SigfHost()
	host.__KeyValueFromString("targetname", "fz_player")
	local ref = Entities.CreateByClassname("info_target")
	ref.__KeyValueFromString("targetname", "fz_ref")
	ref.SetOrigin(Vector(0, 0, 0))
	local tgt = Entities.CreateByClassname("info_target")
	tgt.__KeyValueFromString("targetname", "fz_tgt")
	::fz.eyes = tgt
	local m = Entities.CreateByClassname("logic_measure_movement")
	m.__KeyValueFromString("measuretype", "1")
	m.__KeyValueFromString("measurereference", "fz_ref")
	m.__KeyValueFromString("targetreference", "fz_ref")
	m.__KeyValueFromString("target", "fz_tgt")
	m.__KeyValueFromFloat("targetscale", 1.0)
	EntFireByHandle(m, "SetMeasureTarget", "!player", 0.0, null, null)
	EntFireByHandle(m, "SetMeasureReference", "fz_ref", 0.0, null, null)
	EntFireByHandle(m, "SetTargetReference", "fz_ref", 0.0, null, null)
	EntFireByHandle(m, "Target", "fz_tgt", 0.0, null, null)
	EntFireByHandle(m, "Enable", "", 0.0, null, null)
}

// Two linked real Portal 2 portals on the floor of the platform. Animatronics that step onto one come out of the other.
::fzPortalPair <- function(a, b, life) {
	local loc = function(p) { return format("%.1f %.1f %.1f -90.0 0.0 0.0", p.x, p.y, p.z) }
	local pa = Vector(a.x, a.y, ::fz.floorZ + 1.0)
	local pb = Vector(b.x, b.y, ::fz.floorZ + 1.0)
	SigfSpawn("prop_portal", pa, { LinkageGroupID = 7, PortalTwo = 0 }, life, null, Vector(-90, 0, 0),
		[["SetActivatedState", "1"], ["NewLocation", loc(pa)]])
	SigfSpawn("prop_portal", pb, { LinkageGroupID = 7, PortalTwo = 1 }, life, null, Vector(-90, 0, 0),
		[["SetActivatedState", "1"], ["NewLocation", loc(pb)]])
	::fz.portals.append({ a = a, b = b, die = Time() + life })
}

::fzPortalHop <- function(now) {
	local keep = []
	foreach (pr in ::fz.portals) { if (pr.die > now) keep.append(pr) }
	::fz.portals = keep
	local pp = SigfHost().GetOrigin()
	foreach (pr in ::fz.portals) {
		foreach (A in ::fz.anims) {
			if (!A.alive || A.portalCool > now) continue
			local from = null
			local to = null
			if (::fzDist(A.pos, pr.a) < 55.0) { from = pr.a; to = pr.b }
			else if (::fzDist(A.pos, pr.b) < 55.0) { from = pr.b; to = pr.a }
			if (from == null) continue
			local back = ::fzFlat(pp - to)
			if (back.Length() > 1.0) back.Norm()
			local land = Vector(to.x + back.x * 60.0, to.y + back.y * 60.0, ::fz.floorZ)
			if (!::fzStandable(land)) land = Vector(to.x, to.y, ::fz.floorZ)
			A.portalCool = now + 4.0
			A.stun = now + 1.5
			::fzMove(A, land)
			SigfCaption(A.name + " TOOK A PORTAL!", 2.0)
			::fzStatic(0.35)
		}
	}
}

SigfAfter(0.6, function() {
	SigfCmd("gameinstructor_enable 0")
	SigfCmd("r_drawscreenoverlay 1")
	SigfCmd("hud_quickinfo 0")
	SigfCmd("impulse 100")
	local feet = SigfHost().GetOrigin()
	local ff = TraceLine(feet + Vector(0, 0, 8), feet - Vector(0, 0, 200), null)
	::fz.floorZ = ff < 1.0 ? (feet.z + 8.0 - 208.0 * ff + 1.0) : feet.z
	::fz.center = Vector(SigfHost().GetOrigin().x, SigfHost().GetOrigin().y, ::fz.floorZ)
	::fzEyesInit()
	::fzSpotsInit()
	::fzBaseOverlay()
	::fz.nightStart = Time() + 1.5
	::fz.ready = !::SigfDemoMode
	if (!::SigfDemoMode) {
		SigfIn(0.3, function() { SigfCaption("NIGHT 1  -  12 AM", 3.0) })
		SigfIn(0.8, function() { SigfSound("sigf/phone.wav") })
		SigfIn(3.8, function() { SigfSound("sigf/phoneguy.wav") })
	}
	SigfEvery(0.1, function() { ::fzTick() })
	SigfEvery(16.0, function() {
		if (!::fz.ready || Time() < ::fz.freezeUntil || ::SigfDemoMode) return
		::fzPortalPair(::fzPickSpot(::fzLookYaw() + RandomFloat(-70, 70), 150.0), ::fzPickSpot(::fzLookYaw() + 180.0 + RandomFloat(-70, 70), 150.0), 12.0)
	})
})
printl("FZ mod loaded")
