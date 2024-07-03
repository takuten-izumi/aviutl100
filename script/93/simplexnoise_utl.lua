--[[
	SimplexNoiseをaviutl拡張編集用に改変したモジュールです。
	ビット演算関数ANDはexedit.lua固有のものですので他の環境で使用する場合は変更が必要です。
	lua.5.2以降ならばbit32.bandで。

	Original Source: http://staffwww.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf
	The code there is in java, the original implementation by Ken Perlin




	■使い方■ スクリプト制御で使用する場合はsimplexnoise_utl.luaはexedit.aufと同じ場所に置くといいかも。

	-- ノイズ関数を呼び出す。simplexnoise_utlがグローバル変数として出てきます。
	reuiqre("simplexnoise_utl")
	
	-- 取り敢えず2Dノイズを使用するのでローカル変数noiseに代入 (noise2D, noise3D, noise4D まであります)
	local noise = simplexnoise_utl.noise2D

	-- ノイズの値を決定する位置を用意 (xをobj.timeにしてスクロールするようにした)
	local x,y = obj.time, 0
	
	-- ノイズのスケール。x、yが整数だとブロックノイズになる
	local scale = 0.01

	-- nxにノイズの値(-1～1)が来ます。
	local nx = noise(x * scale, y * scale)
	
	-- 0～1 の範囲に変更したい場合
	nx = (1 + nx) * 0.5
]]

simplexnoise_utl = {}

local AND = AND
local floor = math.floor

local permutation = {151,160,137,91,90,15,
	 131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
	 190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
	 88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
	 77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
	 102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
	 135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
	 5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
	 223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
	 129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
	 251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
	 49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
	 138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
}

local perm = {}
for i=0,255 do
	perm[i]     = permutation[i+1]
	perm[256+i] = permutation[i+1]
end

local grad3 = {
	[0]={1,1,0},{-1,1,0},{1,-1,0},{-1,-1,0},
	{1,0,1},{-1,0,1},{1,0,-1},{-1,0,-1},
	{0,1,1},{0,-1,1},{0,1,-1},{0,-1,-1}
}

local grad4 = {
 	[0]={0,1,1,1}, {0,1,1,-1}, {0,1,-1,1}, {0,1,-1,-1},
 	{0,-1,1,1}, {0,-1,1,-1}, {0,-1,-1,1}, {0,-1,-1,-1},
 	{1,0,1,1}, {1,0,1,-1}, {1,0,-1,1}, {1,0,-1,-1},
 	{-1,0,1,1}, {-1,0,1,-1}, {-1,0,-1,1}, {-1,0,-1,-1},
 	{1,1,0,1}, {1,1,0,-1}, {1,-1,0,1}, {1,-1,0,-1},
 	{-1,1,0,1}, {-1,1,0,-1}, {-1,-1,0,1}, {-1,-1,0,-1},
 	{1,1,1,0}, {1,1,-1,0}, {1,-1,1,0}, {1,-1,-1,0},
 	{-1,1,1,0}, {-1,1,-1,0}, {-1,-1,1,0}, {-1,-1,-1,0}
 }

 local simplex = {
 [0]={0,1,2,3},{0,1,3,2},{0,0,0,0},{0,2,3,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},{1,2,3,0},
 {0,2,1,3},{0,0,0,0},{0,3,1,2},{0,3,2,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},{1,3,2,0},
 {0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},
 {1,2,0,3},{0,0,0,0},{1,3,0,2},{0,0,0,0},{0,0,0,0},{0,0,0,0},{2,3,0,1},{2,3,1,0},
 {1,0,2,3},{1,0,3,2},{0,0,0,0},{0,0,0,0},{0,0,0,0},{2,0,3,1},{0,0,0,0},{2,1,3,0},
 {0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},
 {2,0,1,3},{0,0,0,0},{0,0,0,0},{0,0,0,0},{3,0,1,2},{3,0,2,1},{0,0,0,0},{3,1,2,0},
 {2,1,0,3},{0,0,0,0},{0,0,0,0},{0,0,0,0},{3,1,0,2},{0,0,0,0},{3,2,0,1},{3,2,1,0}
}

local function dot2(g,x,y)
 return g[1]*x + g[2]*y
end

local function dot3(g,x,y,z)
	return g[1]*x + g[2]*y + g[3]*z
end

local function dot4(g,x,y,z,w)
	return g[1]*x + g[2]*y + g[3]*z + g[4]*w
end


-- 2D simplex noise-------------------------------------------------------------
local F2 = 0.5*(math.sqrt(3)-1)
local G2 = (3-math.sqrt(3))/6

function simplexnoise_utl.noise2D(x, y)
	local n0, n1, n2 ---- Noise contributions from the three corners
	local s = (x+y)*F2
	local i = floor(x+s)
	local j = floor(y+s)
	local t = (i+j)*G2
	local X0 = i-t
	local Y0 = j-t
	local x0 = x-X0
	local y0 = y-Y0

	local i1,j1 = 0,1
	if (x0>y0) then
		i1,j1 = 1,0
	end
	local x1 = x0 - i1 + G2
	local y1 = y0 - j1 + G2
	local x2 = x0 - 1 + 2 * G2
	local y2 = y0 - 1 + 2 * G2

	local ii = AND(i , 255)
	local jj = AND(j , 255)
	local gi0 = perm[ii+perm[jj]] % 12
	local gi1 = perm[ii+i1+perm[jj+j1]] % 12
	local gi2 = perm[ii+1+perm[jj+1]] % 12
	local t0 = 0.5 - x0*x0-y0*y0
	if(t0<0) then
		n0 = 0
	else
		n0 = t0*t0*t0*t0 * dot2(grad3[gi0], x0, y0)
	end
	local t1 = 0.5 - x1*x1-y1*y1
	if(t1<0) then
		n1 = 0
	else
		n1 = t1*t1*t1*t1 * dot2(grad3[gi1], x1, y1)
	end
	local t2 = 0.5 - x2*x2-y2*y2
	if(t2<0) then
		n2 = 0
	else
		n2 = t2*t2*t2*t2 * dot2(grad3[gi2], x2, y2)
	end
	return 70 * (n0 + n1 + n2)
end

-- 3D simplex noise-------------------------------------------------------------
local F3 = 1/3
local G3 = 1/6

function simplexnoise_utl.noise3D(x ,y ,z)
	local n0, n1, n2, n3
	local s = (x+y+z)*F3
	local i = floor(x+s)
	local j = floor(y+s)
	local k = floor(z+s)
	local t = (i+j+k)*G3
  	local x0 = x-(i-t)
  	local y0 = y-(j-t)
  	local z0 = z-(k-t)
	local i1, j1, k1
	local i2, j2, k2

	if (x0>=y0) then
		if (y0>=z0) then
			i1,j1,k1, i2,j2,k2 = 1,0,0, 1,1,0 -- X Y Z order
		elseif (x0>=z0) then
			i1,j1,k1, i2,j2,k2 = 1,0,0, 1,0,1 -- X Z Y order
		else
			i1,j1,k1, i2,j2,k2 = 0,0,1, 1,0,1 -- Z X Y order
		end
	else
		if (y0<z0) then
			i1,j1,k1, i2,j2,k2 = 0,0,1, 0,1,1 -- Z Y X order
		elseif (x0<z0) then
			i1,j1,k1, i2,j2,k2 = 0,1,0, 0,1,1 -- Y Z X order
		else
			i1,j1,k1, i2,j2,k2 = 0,1,0, 1,1,0 -- Y X Z order
		end
	end

	local x1 = x0 - i1 + G3
	local y1 = y0 - j1 + G3
	local z1 = z0 - k1 + G3
	local x2 = x0 - i2 + 2*G3
	local y2 = y0 - j2 + 2*G3
	local z2 = z0 - k2 + 2*G3
	local x3 = x0 - 1 + 3*G3
	local y3 = y0 - 1 + 3*G3
	local z3 = z0 - 1 + 3*G3
	local ii = AND(i , 255)
	local jj = AND(j , 255)
	local kk = AND(k , 255)
	local gi0 = perm[ii+perm[jj+perm[kk]]] % 12
	local gi1 = perm[ii+i1+perm[jj+j1+perm[kk+k1]]] % 12
	local gi2 = perm[ii+i2+perm[jj+j2+perm[kk+k2]]] % 12
	local gi3 = perm[ii+1+perm[jj+1+perm[kk+1]]] % 12

	local t0 = 0.5 - x0*x0 - y0*y0 - z0*z0
	if(t0<0)then
		n0 = 0
	else
		t0 = t0 * t0
		n0 = t0 * t0 * dot3(grad3[gi0], x0, y0, z0)
	end
	local t1 = 0.5 - x1*x1 - y1*y1 - z1*z1
	if(t1<0)then
		n1 = 0
	else
		t1 = t1 * t1
		n1 = t1 * t1 * dot3(grad3[gi1], x1, y1, z1)
	end
	local t2 = 0.5 - x2*x2 - y2*y2 - z2*z2
	if(t2<0)then
		n2 = 0
	else
		t2 = t2 * t2
		n2 = t2 * t2 * dot3(grad3[gi2], x2, y2, z2)
	end
	local t3 = 0.5 - x3*x3 - y3*y3 - z3*z3
	if(t3<0)then
		n3 = 0
	else
		t3 = t3 * t3
		n3 = t3 * t3 * dot3(grad3[gi3], x3, y3, z3)
	end
	return 32*(n0 + n1 + n2 + n3)
end

-- 4D simplex noise-------------------------------------------------------------
local F4 = (math.sqrt(5)-1)/4
local G4 = (5-math.sqrt(5))/20

function simplexnoise_utl.noise4D(x,y,z,w)
	 local n0, n1, n2, n3, n4
	 local s = (x + y + z + w) * F4
	 local i = floor(x + s)
	 local j = floor(y + s)
	 local k = floor(z + s)
	 local l = floor(w + s)
	 local t = (i + j + k + l) * G4

	 local x0 = x - (i - t)
	 local y0 = y - (j - t)
	 local z0 = z - (k - t)
	 local w0 = w - (l - t)

	 local c1 = (x0 > y0) and 32 or 0
	 local c2 = (x0 > z0) and 16 or 0
	 local c3 = (y0 > z0) and 8  or 0
	 local c4 = (x0 > w0) and 4  or 0
	 local c5 = (y0 > w0) and 2  or 0
	 local c6 = (z0 > w0) and 1  or 0

	 local c = c1 + c2 + c3 + c4 + c5 + c6
	 local i1, j1, k1, l1
	 local i2, j2, k2, l2
	 local i3, j3, k3, l3

	 i1 = simplex[c][1]>=3 and 1 or 0
	 j1 = simplex[c][2]>=3 and 1 or 0
	 k1 = simplex[c][3]>=3 and 1 or 0
	 l1 = simplex[c][4]>=3 and 1 or 0

	 i2 = simplex[c][1]>=2 and 1 or 0
	 j2 = simplex[c][2]>=2 and 1 or 0
	 k2 = simplex[c][3]>=2 and 1 or 0
	 l2 = simplex[c][4]>=2 and 1 or 0

	 i3 = simplex[c][1]>=1 and 1 or 0
	 j3 = simplex[c][2]>=1 and 1 or 0
	 k3 = simplex[c][3]>=1 and 1 or 0
	 l3 = simplex[c][4]>=1 and 1 or 0

	 local x1 = x0 - i1 + G4
	 local y1 = y0 - j1 + G4
	 local z1 = z0 - k1 + G4
	 local w1 = w0 - l1 + G4
	 local x2 = x0 - i2 + 2*G4
	 local y2 = y0 - j2 + 2*G4
	 local z2 = z0 - k2 + 2*G4
	 local w2 = w0 - l2 + 2*G4
	 local x3 = x0 - i3 + 3*G4
	 local y3 = y0 - j3 + 3*G4
	 local z3 = z0 - k3 + 3*G4
	 local w3 = w0 - l3 + 3*G4
	 local x4 = x0 - 1 + 4*G4
	 local y4 = y0 - 1 + 4*G4
	 local z4 = z0 - 1 + 4*G4
	 local w4 = w0 - 1 + 4*G4

	 local ii = AND(i , 255)
	 local jj = AND(j , 255)
	 local kk = AND(k , 255)
	 local ll = AND(l , 255)
	 local gi0 = perm[ii+perm[jj+perm[kk+perm[ll]]]] % 32
	 local gi1 = perm[ii+i1+perm[jj+j1+perm[kk+k1+perm[ll+l1]]]] % 32
	 local gi2 = perm[ii+i2+perm[jj+j2+perm[kk+k2+perm[ll+l2]]]] % 32
	 local gi3 = perm[ii+i3+perm[jj+j3+perm[kk+k3+perm[ll+l3]]]] % 32
	 local gi4 = perm[ii+1+perm[jj+1+perm[kk+1+perm[ll+1]]]] % 32

	local t0 = 0.6 - x0*x0 - y0*y0 - z0*z0 - w0*w0
	if (t0<0) then
		n0 = 0
	else
		t0 = t0 * t0
		n0 = t0 * t0 * dot4(grad4[gi0], x0, y0, z0, w0)
	end
	local t1 = 0.6 - x1*x1 - y1*y1 - z1*z1 - w1*w1
	if(t1<0) then
		n1 = 0
	else
		t1 = t1 * t1
		n1 = t1 * t1 * dot4(grad4[gi1], x1, y1, z1, w1)
	end
	local t2 = 0.6 - x2*x2 - y2*y2 - z2*z2 - w2*w2
	if (t2<0) then
		n2 = 0
	else
		t2 = t2 * t2
		n2 = t2 * t2 * dot4(grad4[gi2], x2, y2, z2, w2)
	end
	local t3 = 0.6 - x3*x3 - y3*y3 - z3*z3 - w3*w3
	if (t3<0) then
		n3 = 0
	else
		t3 = t3 * t3
		n3 = t3 * t3 * dot4(grad4[gi3], x3, y3, z3, w3)
	end
	local t4 = 0.6 - x4*x4 - y4*y4 - z4*z4 - w4*w4
	if (t4<0) then
		n4 = 0
	else
		t4 = t4 * t4
		n4 = t4 * t4 * dot4(grad4[gi4], x4, y4, z4, w4)
	end
	return 27 * (n0 + n1 + n2 + n3 + n4)
end

-------------------------------------------------------------------------------
-- dimension = 2 or 3 or 4 (noise dimension)
-- v = {x,y[,z,w]}
function simplexnoise_utl.fbm(dimension, v, octaves, lacunarity, gain)
	local noise  = simplexnoise_utl["noise"..(dimension).."D"]
	local x,y,z,w = v[1],v[2],(v[3] or 0),(v[4] or 0)
	octaves = octaves or 3
	lacunarity = lacunarity or 2
	gain = gain or 0.5
	local amplitude = 1
	local frequency = 1
	local sum = 0
	for i = 0, octaves do
		sum = sum + amplitude *
		noise(
			x * frequency + i,
			y * frequency + i,
			z * frequency + i,
			w * frequency + i
		)
		amplitude = amplitude * gain
		frequency = frequency * lacunarity
	end
	return sum
end