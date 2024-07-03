slide_cut_texture_func = function(PATH, INDEX, COLOR, ARG1, ARG3, ifunc, buffer)
	local targ0, targ1, targ2, targ3
	if buffer
	then
		if ARG1 == "r"
		then
			targ0 = "w"
			targ1 = "r"
			targ2 = rikky_module.image("g")
			targ3 = targ2
		else
			targ0 = "cache:image_tempbuffer"
			targ1 = "obj"
			targ2 = "obj"
			targ3 = targ0
		end
		obj.copybuffer("obj", "tmp")
		ifunc(targ0, targ2)
		ifunc(ARG1, ARG3)
	end
	local exte = {".jpg", ".png", ".spa", ".sph", ".jpgl", ".pngl", ".spal", ".sphl" , ".jpgc", ".pngc", ".spac", ".sphc" , ".jpgcl", ".pngcl", ".spacl", ".sphcl"}
	local w, h
	local ow, oh = obj.getpixel()
	obj.copybuffer("tmp", "obj")
	obj.setoption("drawtarget", "tempbuffer")
	local suc = 0
	for i = 1, 16
	do
		obj.load("image", PATH .. INDEX .. exte[i])
		w, h = obj.getpixel()
		if 0 < w
		then
			if i < 3
			then
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				suc = 1
				break
			elseif i < 4
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.setoption("blend", 1)
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 5
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.setoption("blend", 3)
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 7
			then
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.draw()
				suc = 1
				break
			elseif i < 8
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.setoption("blend", 1)
				obj.draw()
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 9
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.setoption("blend", 3)
				obj.draw()
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 11
			then
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				suc = 1
				break
			elseif i < 12
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				obj.setoption("blend", 1)
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 13
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				obj.setoption("blend", 3)
				obj.drawpoly(-ow * .5, -oh * .5, 0, ow * .5, -oh * .5, 0, ow * .5, oh * .5, 0, -ow * .5, oh * .5, 0)
				obj.setoption("blend", 0)
				suc = 1
				break
			elseif i < 15
			then
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.draw()
				suc = 1
				break
			elseif i < 16
			then
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.setoption("blend", 1)
				obj.draw()
				obj.setoption("blend", 0)
				suc = 1
				break
			else
				obj.load("image", PATH .. INDEX .. exte[i])
				obj.effect("’PF‰»", "color", COLOR, "‹P“x‚ð•ÛŽ‚·‚é", 1)
				local y = math.ceil(ow / w)
				local t = math.ceil(oh / h)
				obj.effect("‰æ‘œƒ‹[ƒv", "c‰ñ”", t, "‰¡‰ñ”", y)
				obj.setoption("blend", 3)
				obj.draw()
				obj.setoption("blend", 0)
				suc = 1
				break
			end
		end
	end
	if suc == 1
	then
		ifunc(ARG1, ARG3)
		obj.effect("”½“]", "“§–¾“x”½“]", 1)
		obj.setoption("blend", "alpha_sub")
		obj.draw()
		obj.setoption("blend", 0)
		if buffer
		then
			local targ4, targ5, targ6, targ7
			if ARG1 == "r"
			then
				targ4 = "w"
				targ5 = "r"
				targ6 = rikky_module.image("g")
				targ7 = targ6
			else
				targ4 = "cache:image_maketexture"
				targ5 = "obj"
				targ6 = "obj"
				targ7 = targ4
			end
			obj.copybuffer("obj", "tmp")
			ifunc(targ4, targ6)
			ifunc(targ1, targ3)
			obj.copybuffer("tmp", "obj")
			ifunc(targ5, targ7)
			if targ0 == "w"
			then
				ifunc("c", targ2)
				ifunc("c", targ6)
			end
		else
			obj.copybuffer("obj", "tmp")
			obj.setoption("drawtarget", "framebuffer")
		end
	else
		if buffer
		then
			ifunc(targ1, targ3)
			obj.copybuffer("tmp", "obj")
			ifunc(ARG1, ARG3)
		else
			ifunc(ARG1, ARG3)
			obj.setoption("drawtarget", "framebuffer")
		end
	end
	return suc
end

local GetPolyPos = function(x, y, rad, xy)
	local vx = math.cos(rad)
	local vy = math.sin(rad)
	local pos = {}
	for i = 1, 4
	do
		if 0 < vx * (xy[i].y - y) - vy * (xy[i].x - x)
		then
			pos[#pos + 1] = i
		end
	end
	if #pos == 0
	then
		return 0, {}
	end
	if #pos == 4
	then
		return 1, {xy[1].x, xy[1].y, xy[2].x, xy[2].y, xy[3].x, xy[3].y, xy[4].x, xy[4].y}
	else
		local SE = {}
		local nx, ny, xx, yy, z, t, f
		for i = 1, 4
		do
			z = (i % 4) + 1 
			nx = xy[z].x - xy[i].x
			ny = xy[z].y - xy[i].y
			z = vx * ny - vy * nx
			if z ~= 0
			then
				t = (vy * (xy[i].x - x) - vx * (xy[i].y - y)) / z
				if 0 <= t and t <= 1
				then
					xx = xy[i].x + nx * t
					yy = xy[i].y + ny * t
					f = 1
					for j = 1, #SE
					do
						if xx == SE[j].x and yy == SE[j].y
						then
							f = 0
							break;
						end
					end
					if f == 1
					then
						SE[#SE + 1] = {x = xx, y = yy}
					end
				end
			end
		end
		if #SE == 1
		then
			SE[2] = {x = SE[1].x, y = SE[1].y}
		end
		f = #pos
		local k = 1
		while k <= f
		do
			for j = 1, #SE
			do
				if xy[pos[k]].x == SE[j].x and xy[pos[k]].y == SE[j].y
				then
					for i = k, f
					do
						pos[i] = pos[i + 1]
					end
					f = f - 1
					k = k - 1
					break
				end
			end
			k = k + 1
		end
		if f == 1
		then
			f = pos[1]
			return 2, {xy[f].x, xy[f].y, SE[1].x, SE[1].y, SE[2].x, SE[2].y}
		elseif f == 2
		then
			local st = {{x = SE[1].x, y = SE[1].y}, {x = SE[2].x, y = SE[2].y}}
			f = pos[1]
			st[3] = {x = xy[f].x, y = xy[f].y}
			f = pos[2]
			st[4] = {x = xy[f].x, y = xy[f].y}
			st[5] = {}
			f = 1
			local x0 = st[1].x
			for i = 2, 4
			do
				if x0 < st[i].x
				then
					x0 = st[i].x
					f = i
				end
			end
			x0 = st[f].x
			local y0 = st[f].y
			for i = f, 4
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			f = 1
			t = math.atan2(st[1].y - y0, st[1].x - x0)
			if t <= 0
			then
				t = t + math.pi * 2
			end
			for i = 2, 3
			do
				z = math.atan2(st[i].y - y0, st[i].x - x0)
				if z <= 0
				then
					z = z + math.pi * 2
				end
				if z < t
				then
					f = i
					t = z
				end 
			end
			local x1 = st[f].x
			local y1 = st[f].y
			for i = f, 3
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			f = 1
			t = math.atan2(st[1].y - y1, st[1].x - x1)
			if t <= 0
			then
				t = t + math.pi * 2
			end
			z = math.atan2(st[2].y - y1, st[2].x - x1)
			if z <= 0
			then
				z = z + math.pi * 2
			end
			if z < t
			then
				f = 2
			end 
			local x2 = st[f].x
			local y2 = st[f].y
			for i = f, 2
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			local x3 = st[1].x
			local y3 = st[1].y
			return 1, {x0, y0, x1, y1, x2, y2, x3, y3}
		else
			local st = {{x = SE[1].x, y = SE[1].y}, {x = SE[2].x, y = SE[2].y}}
			f = pos[1]
			st[3] = {x = xy[f].x, y = xy[f].y}
			f = pos[2]
			st[4] = {x = xy[f].x, y = xy[f].y}
			f = pos[3]
			st[5] = {x = xy[f].x, y = xy[f].y}
			st[6] = {}
			f = 1
			local x0 = st[1].x
			for i = 2, 5
			do
				if x0 < st[i].x
				then
					x0 = st[i].x
					f = i
				end
			end
			x0 = st[f].x
			local y0 = st[f].y
			for i = f, 5
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			f = 1
			t = math.atan2(st[1].y - y0, st[1].x - x0)
			if t <= 0
			then
				t = t + math.pi * 2
			end
			for i = 2, 4
			do
				z = math.atan2(st[i].y - y0, st[i].x - x0)
				if z <= 0
				then
					z = z + math.pi * 2
				end
				if z < t
				then
					f = i
					t = z
				end 
			end
			local x1 = st[f].x
			local y1 = st[f].y
			for i = f, 4
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			f = 1
			t = math.atan2(st[1].y - y1, st[1].x - x1)
			if t <= 0
			then
				t = t + math.pi * 2
			end
			for i = 2, 3
			do
				z = math.atan2(st[i].y - y1, st[i].x - x1)
				if z <= 0
				then
					z = z + math.pi * 2
				end
				if z < t
				then
					f = i
					t = z
				end 
			end
			local x2 = st[f].x
			local y2 = st[f].y
			for i = f, 3
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			f = 1
			t = math.atan2(st[1].y - y2, st[1].x - x2)
			if t <= 0
			then
				t = t + math.pi * 2
			end
			z = math.atan2(st[2].y - y2, st[2].x - x2)
			if z <= 0
			then
				z = z + math.pi * 2
			end
			if z < t
			then
				f = 2
			end 
			local x3 = st[f].x
			local y3 = st[f].y
			for i = f, 2
			do
				st[i].x = st[i + 1].x
				st[i].y = st[i + 1].y
			end
			local x4 = st[1].x
			local y4 = st[1].y
			return 3, {x0, y0, x1, y1, x2, y2, x3, y3, x4, y4}
		end
	end
end

slide_cut_slanting_func = function(NX, NY, X, Y, PI0, PI1, PI, ARG1, ifunc, buffer)
	local oarg0, oarg1, oarg2, oarg3
	if ARG1 == "r"
	then
		oarg0 = "w"
		oarg1 = "r"
		oarg2 = rikky_module.image("g")
		oarg3 = oarg2
	else
		oarg0 = "cache:image_original"
		oarg1 = "obj"
		oarg2 = oarg1
		oarg3 = oarg0
	end
	ifunc(oarg0, oarg2)
	local w, h = obj.getpixel()
	local xy = {}
	xy[1] = {x = -w * .5, y = -h * .5}
	xy[2] = {x = -xy[1].x, y = xy[1].y}
	xy[3] = {x = xy[2].x, y = -xy[1].y}
	xy[4] = {x = xy[1].x, y = xy[3].y}
	local tt = {{}, {}, {}, {}}
	tt[1].type, tt[1].pos = GetPolyPos(-NY + X, NX + Y, PI, xy)
	tt[2].type, tt[2].pos = GetPolyPos(-NY + X, NX + Y, PI + PI0, xy)
	tt[3].type, tt[3].pos = GetPolyPos(NY + X, -NX + Y, math.pi + PI, xy)
	tt[4].type, tt[4].pos = GetPolyPos(NY + X, -NX + Y, math.pi + PI + PI1, xy)
	local targ0, targ1, targ2, targ3
	if buffer
	then
		if ARG1 == "r"
		then
			targ0 = "w"
			targ1 = "r"
			targ2 = rikky_module.image("g")
			targ3 = targ2
		else
			targ0 = "cache:image_tempbuffer"
			targ1 = "obj"
			targ2 = targ1
			targ3 = targ0
		end
		obj.copybuffer("obj", "tmp")
		ifunc(targ0, targ2)
		ifunc(oarg1, oarg3)
	end
	obj.copybuffer("tmp", "obj")
	obj.setoption("drawtarget", "tempbuffer")
	obj.load("figure", "ŽlŠpŒ`", 0, 2)
	obj.setoption("blend", "alpha_sub")
	for i = 1, 2
	do
		if tt[i].type == 1
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0, tt[i].pos[7], tt[i].pos[8], 0)
		elseif tt[i].type == 2
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0)
		elseif tt[i].type == 3
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0)
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[5], tt[i].pos[6], 0, tt[i].pos[7], tt[i].pos[8], 0, tt[i].pos[9], tt[i].pos[10], 0)
		end
	end
	obj.setoption("blend", 0)
	local targ4, targ5, targ6, targ7
	if ARG1 == "r"
	then
		targ4 = "w"
		targ5 = "r"
		targ6 = rikky_module.image("g")
		targ7 = targ6
	else
		targ4 = "cache:image_slantR"
		targ5 = "obj"
		targ6 = targ5
		targ7 = targ4
	end
	obj.copybuffer("obj", "tmp")
	ifunc(targ4, targ6)
	ifunc(oarg1, oarg3)
	obj.copybuffer("tmp", "obj")
	obj.load("figure", "ŽlŠpŒ`", 0, 2)
	obj.setoption("blend", "alpha_sub")
	for i = 3, 4
	do
		if tt[i].type == 1
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0, tt[i].pos[7], tt[i].pos[8], 0)
		elseif tt[i].type == 2
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0)
		elseif tt[i].type == 3
		then
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[3], tt[i].pos[4], 0, tt[i].pos[5], tt[i].pos[6], 0)
			obj.drawpoly(tt[i].pos[1], tt[i].pos[2], 0, tt[i].pos[5], tt[i].pos[6], 0, tt[i].pos[7], tt[i].pos[8], 0, tt[i].pos[9], tt[i].pos[10], 0)
		end
	end
	obj.setoption("blend", "alpha_add")
	ifunc(targ5, targ7)
	obj.draw()
	obj.copybuffer("obj", "tmp")
	obj.setoption("blend", 0)
	ifunc(targ4, targ6)
	if buffer
	then
		ifunc(targ1, targ3)
		obj.copybuffer("tmp", "obj")
		ifunc(targ5, targ7)
		if ARG1 == "r"
		then
			ifunc("c", targ2)
			ifunc("c", targ6)
			ifunc("c", oarg2)
		end
	else
		if ARG1 == "r"
		then
			ifunc("c", targ6)
			ifunc("c", oarg2)
		end
		obj.setoption("drawtarget", "framebuffer")
	end
end
