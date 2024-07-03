
--[[

����̈ʒu���狅��ɉ����o���t�B�[���h�����܂��B
��{�I�ɑ��΍��W�𑀍삵�܂��B
�X�N���v�g�Ŏg�p����ꍇ��require�ŌĂяo���ĉ������B

��x���̕ϐ��ɑ�����邩�A
local Field = require("SPfield")
Field(pos,layer,radius,strength,ret)

�������͒��ڈ�����n���Ċ֐������s���܂��B
require("SPfield")(pos,layer,radius,strength,ret)


SphericalField�X�N���v�g�Ƃ͏����������Ⴂ�܂��B
------�����̐���-------

SPfield([pos,layer,radius,strength,ret])

pos
  �����o�������I�u�W�F�N�g�̍��W��{x,y,z} �Ŏw�肵�܂��B
  nil�̏ꍇ�͌��݂̃I�u�W�F�N�g�̑��΍��W������܂��B

layer (�ȗ���={0,0,0})
  �����o���t�B�[���h�̈ʒu�����C���[�Ŏw�肵�܂��B
  �e�[�u��{x,y,z}�Œ��ڍ��W���w�肷�邱�Ƃ��ł��܂��B
  nil����{0,0,0} �ɂȂ�܂��B

radius (�ȗ���=200)
  �����o���t�B�[���h�̃T�C�Y���w�肵�܂��B
  nil����layer�Ŏw�肵���I�u�W�F�N�g�̊g�嗦���g�p���܂��B
  �}�C�i�X�ɂ���Ƌz�����܂��B

strength(�ȗ���=100)
  �e���x�����Ŏw�肵�܂��B

container(�ȗ���=0)
  1�̏ꍇ�������݃t�B�[���h�ɂȂ�܂��B

ret (�ȗ���=false)
  0�ȊO�̉��炩�̒l������Ɩ߂�l�݂̂ɂȂ�܂��B
  obj.draw�ő�ʂɕ`�悷��ꍇ�Ɏg�p���܂��B
  x,y,z,�P�ʉ����ꂽ����, �̎l��Ԃ��܂��B
  �������ȗ����ł����Ă��߂�l��Ԃ��܂��B

]]




local Field=function(pos,layer,radius,strength,container,ret)

  local r,f,L = radius ,strength or 100,layer
  f = f*.01
  --f=math.max(-1,math.min(2, f))
  local gv=obj.getvalue
  local pos=pos or {obj.ox,obj.oy,obj.oz}
  local ox,oy,oz = unpack(pos)
  local xx,yy,zz = (ox+obj.x),(oy+obj.y),(oz+obj.z)
  local x,y,z = 0,0,0

  if string.find(tostring(L),"table:") then
    x,y,z = unpack(L)
  elseif L==nil then
    return ox,oy,oz,1
  else
    L="layer"..L
    x,y,z=gv(L..".x"),gv(L..".y"),gv(L..".z")
    r = radius or gv(L..".zoom")/2
    if gv(L..".zoom")==nil then
      return ox,oy,oz,1
      --x,y,z,r=0,0,0,0
    end
  end
  r = r or 200
  local xa,ya,za=xx-x,yy-y,zz-z
  local xb,yb,zb=x-xx,y-yy,z-zz

  if r<0 then
    xa,ya,za,xb,yb,zb = xb,yb,zb,xa,ya,za
  end
  r = math.abs(r)
  local l=(xa*xa + ya*ya + za*za)^.5
  local nx,ny,nz = xb/l,yb/l,zb/l
  local d = ( l<r and (1 - l/r) ) or 0
  d = d*f

if container==1 then
  if (l>r) then  --�������]+feather=0����Container��
    ox = (xb-nx*r)*(f) + ox 
    oy = (yb-ny*r)*(f) + oy 
    oz = (zb-nz*r)*(f) + oz 
    if ret==nil or ret==0 then
      obj.ox,obj.oy,obj.oz = ox,oy,oz
    end
  end
  return ox,oy,oz,d
else
  if (l<r) then  --�������]+feather=0����Container��
    ox = (xb-nx*r)*(f) + ox 
    oy = (yb-ny*r)*(f) + oy 
    oz = (zb-nz*r)*(f) + oz 
    if ret==nil or ret==0 then
      obj.ox,obj.oy,obj.oz = ox,oy,oz
    end
  end
  return ox,oy,oz,d
end

end

return Field

