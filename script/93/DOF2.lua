--[[
�Ȃ񂿂����Depth of Field �U��ʊE�[�x2 �̊֐��łł��B
exedit.auf�Ɠ����ꏊ��DOF2.lua��u���ƃX�N���v�g����ȂǂŌĂяo���܂��B
��x���炩�̕ϐ��Ɋ֐������Ă���
local dof = require("DOF2")
���s���Ă��������B
dof(pos,blur,focusmode,focalpoint,aperture,alpha,fade,lens,mode)

���ڎg�p���邱�Ƃ��ł��܂��B
require("DOF2")(nil,20)

���łɂق��̃I�u�W�F�N�g�ŋU��ʊE�[�x2���g�p���Ă�ꍇ�A��������nil�ɂ��Ă�����
�U��ʊE�[�x2�̃p�����[�^�[�ƘA�����܂��B�Ȃ̂ŋU��ʊE�[�x2�����s�Ă���ꍇ�̓X�N���v�g�����
require("DOF2")()
�œ��삵�܂��B


DOF2( [pos, blur, focusmode, focalpoint, aperture, fade , lens, mode])

	pos = {x,y,z} (�G�t�F�N�g���|����Ώۂ̍��W) �����ŃI�u�W�F�N�g�̍��W+���΍��W�������ŉ��Z�����̂Œʏ�� nil �� {0,0,0}��OK�ł�
	blur = �����Y�u���[�̋��x�A �U��ʊE�[�x2�𑼂̃I�u�W�F�N�g�Ŏg�p���Ă���ꍇ, nil �ɂ���Ƃ�����ƘA�����܂��B(0�`100)���x
	focusmode = true ���Əœ_��ڕW�ɌŒ� (0��false��������܂�)
	focalpoint = �œ_����     (-5000�`5000)���x
	aperture = �œ_�̍����͈� (0�`5000)���x
	fade = ���߂�l�̒l(0�`1�͈̔�) ���ω����鋗�������Ŏw�肵�܂��B100���ƃu���[�Ɠ��l�̕ω��ł��B 100��菬�����Ƌ}���Ɍ������܂��B (1�`500)
	lens = 0���ƒʏ�̂ڂ������g�p�A �f�t�H���g��1�Ń����Y�u���[�ł��B
	mode = 1���ƒP���ȃJ�����Ƃ̋����v�Z�ɂȂ�܂��B �f�t�H���g��0

	��1�߂�l�� �����x�p��1~0�̒l, ��2�߂�l��Depth�l 0~ ��Ԃ��܂��Bblur��0�ɂ���Ɩ߂�l�������o���܂��B
	local alpha,depth = require("DOF2")(nil,0)


--�g�p�� (�X�N���[���Ƀh�b�g��`�悷��X�N���v�g)---------------------------


--�c����
local nx,ny=16,9

--DOF2���Ăяo��
local Dof = require("DOF2")

--�����ł͌��摜�����z�o�b�t�@�ɑޔ����ă��[�v����copybuffer�ŌĂяo���d����
--�摜�����H����G�t�F�N�g�̏ꍇ�d�ˊ|���ɂȂ�Ȃ��l�Ƀ��[�v����load��copybuffer�ŉ摜�����������܂��B
if (obj.w+obj.h)/2 < 20 then
obj.copybuffer("tmp","obj")
else
obj.setfont("koruri",20)
obj.load("�摜�T�C�Y���傫�����܂�")
return
end

--�J����������
obj.setoption("billboard",3)

local sw,sh=obj.screen_w *1.0,obj.screen_h*1.0
for i=0,nx-1,1/2 do
for j=0,ny-1,1/2 do
--local frm=(i*ny*nz)*2+(j*nz)*2+k*2
local frm=(i*ny)*2+(j)*2
local R=rand(-1000,1000,-1,frm) *.001
local x=sw/(nx-1)*i - sw/2
local y=sh/(ny-1)*j - sh/2
local z=R*(obj.time)*50

-- ���z�o�b�t�@����I���W�i���摜���Ă�
obj.copybuffer("obj","tmp")
-- �G�t�F�N�g�������߂�l�œ����x�p�̒l���擾
local alp = Dof({x,y,z},10)
-- draw�ɓ����x��K�p
obj.draw(x,y,z, (R+1)/2 , alp)
end
end

------------------------------------

]]











local DOF2 = function(...)
 local T={...}
 local blur,focusmode,focalpoint,aperture,alpha,fade,lens,mode
 local pos = T[1] or {0,0,0} --{obj.x+obj.ox, obj.y+obj.oy, obj.z+obj.oz}
 local objectpos = {obj.x+obj.ox, obj.y+obj.oy, obj.z+obj.oz}

if DOF_TBL and T[2]==nil then
 	  blur,focusmode,focalpoint,aperture,alpha,fade,lens,mode = unpack(DOF_TBL)
		alpha = 0
else
	 blur       = T[2] or 0
	 focusmode  = T[3] or false
	 focalpoint = T[4] or 0
	 aperture   = T[5] or 0
	 alpha      = 0 --T[6] or 0
	 fade       = T[6] or 200
	 lens       = T[7] or 1
	 mode       = T[8] or 0
  if focusmode==0 then focusmode=false end
end


  local maxblur = blur * 2

	local Add = function(a,b)
		return {a[1]+b[1], a[2]+b[2], a[3]+b[3]}
	end

	local Sub = function(a,b)
		return {a[1]-b[1], a[2]-b[2], a[3]-b[3]}
	end

	local Scale = function(v,scale)
		return {v[1]*scale, v[2]*scale, v[3]*scale}
	end

	local Length = function(v)
		local x,y,z = v[1],v[2],v[3]
		return math.sqrt((x*x)+(y*y)+(z*z))
	end

	local Normalize = function(v)
		local x,y,z = v[1],v[2],v[3]
		local l = 1/math.sqrt(x*x + y*y + z*z)
		return {x*l, y*l, z*l}
	end

	local Dot = function(a,b)
		return a[1]*b[1] + a[2]*b[2] + a[3]*b[3]
	end

	local Cross = function(v0,v1)
		local x0,y0,z0 = v0[1],v0[2],v0[3]
		local x1,y1,z1 = v1[1],v1[2],v1[3]
		return {
		(y0 * z1) - (z0 * y1),
		(z0 * x1) - (x0 * z1),
		(x0 * y1) - (y0 * x1)
		}
	end

	local zoom = obj.getvalue("zoom")*.01
	local P0 = Add(pos,objectpos)
	local cam = obj.getoption("camera_param")
	local camT = {cam.tx,cam.ty,cam.tz}
	local camP = {cam.x,cam.y,cam.z}
	local camV = Sub(camT,camP)
	local screen = 1024 -- or cam.d

	--<< ���S�_���g������]���܂݂����Ȃ��ꍇ�͂����𖳌���//////////////////
	local ROT = function(p)
		local toRad=math.pi/180
		local x,y,z = p[1],p[2],p[3]
		local rx,ry,rz = obj.rx*toRad,obj.ry*toRad,obj.rz*toRad
		local x0=x*math.cos(rz)-y*math.sin(rz)
		local y0=x*math.sin(rz)+y*math.cos(rz)
		local z0=z*math.cos(ry)-x0*math.sin(ry)
		return {
			z*math.sin(ry)+x0*math.cos(ry),
			y0*math.cos(rx)-z0*math.sin(rx),
			y0*math.sin(rx)+z0*math.cos(rx)
		}
	end

	local C1 = ROT({obj.cx*zoom,obj.cy*zoom,obj.cz})
	      P0 = Sub(P0,C1)
	--///////////////////////////////////////////////////////�����܂� >>

	local P1 = Sub(P0,camP)
	local d = Length(camV)
	if d<1 then d=1 end

	local L
	local N = Scale(camV,1/d) --Normalize(camV)
	local P = Scale(N,(focusmode and d) or screen)

	if mode==1 then
	 L = Length(Sub(camP,P0)) - (focusmode and d or screen )
	else
	 L = Dot(Sub(P1,P),N)
	end

  	local R = L - focalpoint
  	local D = math.abs(R)
	      D = (D-aperture)/screen
	if D<0 then D=0 end


	local fdp = fade*.01
	local alp = math.max(0,math.min(fdp-D,fdp)) / fdp
	if alp>1 then alp=1 end
	local Blur = math.abs(blur) * math.min(D,3)

	if (obj.alpha*obj.getvalue("alpha") > .0001 ) then

	if lens==0 then
	obj.effect("�ڂ���","�͈�",Blur)
	else
	obj.effect("�����Y�u���[","�͈�",Blur,"�T�C�Y�Œ�",0)
	end

	end
	return alp,D,R
end

return DOF2
