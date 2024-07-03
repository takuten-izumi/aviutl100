uf = {}
------------------------------
--array
------------------------------
local function idx_shift(idx,num,zure)
  if( num <= 0 ) then 
    return -1
  end
  
  local ii = idx-math.floor(num*zure) -- error line 49
  local lb = 1
  local ub = num
  
  --debug_print("check:i="..idx.." ii="..ii.." num="..num.." lb-1="..(lb-1).." ub+1="..(ub+1))
  while ( ii <= lb-1 ) do
    ii = ii + num
  end
  while ( ii >= ub+1 ) do
    ii = ii - num
  end
  
  return ii
end
local function idx_swing(idx,swing_max)
  return math.abs( ((idx+swing_max) % (swing_max*2)) - swing_max )
end
------------------------------
--draw
------------------------------
local function get_chain_code(x1,y1,x2,y2)
  local dx,dy = x2-x1,y2-y1
  if( dy < 0 ) then
    if( dx < 0 ) then
      return 3
    elseif( dx > 0 ) then
      return 1
    else
      return 2
    end
  elseif(dy > 0 ) then
    if( dx < 0 ) then
      return 5
    elseif( dx > 0 ) then
      return 7
    else
      return 6
    end
  else
    if( dx < 0 ) then
      return 4
    elseif( dx > 0 ) then
      return 0
    else
      return -1
    end
  end
end
local function create_matrix_shift(a,b,c)
  return {
    1,0,0,0, 
    0,1,0,0, 
    0,0,1,0, 
    a,b,c,1}
end
local function create_matrix_scale(sx,sy,sz)
  return {
    sx, 0, 0, 0, 
     0,sy, 0, 0, 
     0, 0,sz, 0, 
     0, 0, 0, 1}
end
local function create_matrix_rotate_x(rad)
  local c,s = math.cos(rad),math.sin(rad)
  return {
     1, 0, 0, 0, 
     0, c, s, 0,
     0,-s, c, 0,
     0, 0, 0, 1}
end
local function create_matrix_rotate_y(rad)
  local c,s = math.cos(rad),math.sin(rad)
  return {
     c, 0,-s, 0, 
     0, 1, 0, 0,
     s, 0, c, 0,
     0, 0, 0, 1}
end
local function create_matrix_rotate_z(rad)
  local c,s = math.cos(rad),math.sin(rad)
  return {
     c,s,0,0, 
    -s,c,0,0,
     0,0,1,0,
     0,0,0,1}
end
local function mul(m1,m2)
  return {
    m1[ 1]*m2[ 1] + m1[ 2]*m2[ 5] + m1[ 3]*m2[ 9] + m1[ 4]*m2[13], 
    m1[ 1]*m2[ 2] + m1[ 2]*m2[ 6] + m1[ 3]*m2[10] + m1[ 4]*m2[14], 
    m1[ 1]*m2[ 3] + m1[ 2]*m2[ 7] + m1[ 3]*m2[11] + m1[ 4]*m2[15], 
    m1[ 1]*m2[ 4] + m1[ 2]*m2[ 8] + m1[ 3]*m2[12] + m1[ 4]*m2[16],
    
    m1[ 5]*m2[ 1] + m1[ 6]*m2[ 5] + m1[ 7]*m2[ 9] + m1[ 8]*m2[13], 
    m1[ 5]*m2[ 2] + m1[ 6]*m2[ 6] + m1[ 7]*m2[10] + m1[ 8]*m2[14], 
    m1[ 5]*m2[ 3] + m1[ 6]*m2[ 7] + m1[ 7]*m2[11] + m1[ 8]*m2[15], 
    m1[ 5]*m2[ 4] + m1[ 6]*m2[ 8] + m1[ 7]*m2[12] + m1[ 8]*m2[16],
    
    m1[ 9]*m2[ 1] + m1[10]*m2[ 5] + m1[11]*m2[ 9] + m1[12]*m2[13], 
    m1[ 9]*m2[ 2] + m1[10]*m2[ 6] + m1[11]*m2[10] + m1[12]*m2[14], 
    m1[ 9]*m2[ 3] + m1[10]*m2[ 7] + m1[11]*m2[11] + m1[12]*m2[15], 
    m1[ 9]*m2[ 4] + m1[10]*m2[ 8] + m1[11]*m2[12] + m1[12]*m2[16],
    
    m1[13]*m2[ 1] + m1[14]*m2[ 5] + m1[15]*m2[ 9] + m1[16]*m2[13], 
    m1[13]*m2[ 2] + m1[14]*m2[ 6] + m1[15]*m2[10] + m1[16]*m2[14], 
    m1[13]*m2[ 3] + m1[14]*m2[ 7] + m1[15]*m2[11] + m1[16]*m2[15], 
    m1[13]*m2[ 4] + m1[14]*m2[ 8] + m1[15]*m2[12] + m1[16]*m2[16]
  }
end
local function mat(pos, mat)
  local x=pos[1]
  local y=pos[2]
  local z=pos[3] or 0
  local l=pos[4] or 1
  return {x*mat[1]+y*mat[5]+z*mat[9]+l*mat[13], x*mat[2]+y*mat[6]+z*mat[10]+l*mat[14], x*mat[3]+y*mat[7]+z*mat[11]+l*mat[12]}
end
-- äeéÌÉpÉâÉÅÅ[É^Çé©ëOåvéZÇ∑ÇÈobj.drawpoly()
-- x,y,z ç¿ïW
-- width ïù
-- height çÇÇ≥
-- center [1][2] íÜêSX,Y
-- rad [1][2][3]X,Y,ZâÒì]
-- aspect ägí£ï`âÊÇÃècâ°î‰
-- zoom ïWèÄï`âÊÇÃägëÂó¶
-- alpha ïsìßñæó¶
local function draw_poly(x,y,z,width,height,center,rad,aspect,zoom,alpha)
  if(center==nil) then
    center = {0,0}
  end
  if(rad==nil) then
    rad = {0,0,0}
  end
  if(aspect==nil) then
    aspect = 0
  end
  if(zoom==nil) then
    zoom = 1
  end
  if(alpha==nil) then
    alpha = 1
  end
  
  local obj_w,obj_wh = width,width*0.5
  local obj_h,obj_hh = height,height*0.5
  
  local center_x,center_y = center[1],center[2]
  local aspect_x,aspect_y = 0,0
  
  if(aspect>0) then
    aspect_x = obj_w*aspect*0.5
    center_x = center[1] - center[1]*aspect
  else
    aspect_y = obj_h*aspect*0.5
    center_y = center[2] - center[2]*aspect
  end
  
  --äÓñ{à íu
  local base_point = {
    {-obj_wh+aspect_x-center_x,-obj_hh-aspect_y-center_y,0},  --ï`âÊêÊ ç∂è„
    { obj_wh-aspect_x-center_x,-obj_hh-aspect_y-center_y,0},  --ï`âÊêÊ âEè„
    { obj_wh-aspect_x-center_x, obj_hh+aspect_y-center_y,0},  --ï`âÊêÊ âEâ∫
    {-obj_wh+aspect_x-center_x, obj_hh+aspect_y-center_y,0}   --ï`âÊêÊ ç∂â∫
  }
  
  local matrix
  --Zé≤âÒì]
  matrix = create_matrix_rotate_z(rad[3])
  for j = 1, 4 do
    base_point[j] = mat(base_point[j],matrix)
  end
  --Xé≤âÒì]
  matrix = create_matrix_rotate_z(rad[1])
  for j = 1, 4 do
    base_point[j] = mat(base_point[j],matrix)
  end
  --Yé≤âÒì]
  matrix = create_matrix_rotate_z(rad[2])
  for j = 1, 4 do
    base_point[j] = mat(base_point[j],matrix)
  end
  
  --ägëÂó¶
  matrix = create_matrix_scale(zoom,zoom,1)
  for j = 1, 4 do
    base_point[j] = mat(base_point[j],matrix)
  end

  --x,y,z
  matrix = create_matrix_shift(x,y,z)
  for j = 1, 4 do
    base_point[j] = mat(base_point[j],matrix)
  end
  
  obj.drawpoly(
    base_point[1][1],base_point[1][2],base_point[1][3], --ï`âÊêÊ ç∂è„
    base_point[2][1],base_point[2][2],base_point[2][3], --ï`âÊêÊ âEè„
    base_point[3][1],base_point[3][2],base_point[3][3], --ï`âÊêÊ âEâ∫
    base_point[4][1],base_point[4][2],base_point[4][3], --ï`âÊêÊ ç∂â∫
    
        0,    0,               --ï`âÊå≥ ç∂è„
    obj_w,    0,               --ï`âÊå≥ âEè„
    obj_w,obj_h,               --ï`âÊå≥ âEâ∫
        0,obj_h,               --ï`âÊå≥ ç∂â∫
    alpha
  )
  --[[
  debug_print(string.format("(%f,%f)-(%f,%f)-(%f,%f)-(%f,%f) width:%f height:%f",
    base_point[1][1],base_point[1][2],
    base_point[2][1],base_point[2][2],
    base_point[3][1],base_point[3][2],
    base_point[4][1],base_point[4][2],
    obj_w,obj_h
  ))
  ]]

end
local function draw_figure(x1,y1,x2,y2,width)
  local dx,dy = x2-x1,y2-y1
  local rad = math.atan2(dy,dx)
  
  if( dx==0 and dy==0 ) then
    return
  else
    rad = rad + math.pi*0.5
  end
  
  local len = math.sqrt(dx^2+dy^2)
  local height = width
  local zoom = len/width
  
  local aspect = 0
  if(width>len) then
  else
    aspect=1-width/len
  end
  
  draw_poly(x1,y1,0,width,height,{0,height*0.5},{0,0,rad},aspect,zoom,1)
end

------------------------------
--core
------------------------------
local function is_upward_script(global_value,script_name)
  local no=-1
  repeat
    local name = obj.getoption("script_name",no,true)
    if( name == script_name ) then
      if( global_value ) then
        if( global_value[obj.layer] ) then
          return true
        end
      end
    end
    no=no-1
  until name=="" or no==-100 --ÉXÉNÉäÉvÉgà»äOÇ™èoÇƒÇ´ÇΩÇËÅA-100Ç‹Ç≈Ç¢Ç¡ÇΩÇÁî≤ÇØÇÈ
  return false
end

local func = {
  core = function()
    uf.core = {
      is_upward_script = is_upward_script
    }
  end,
  array = function()
    uf.array = {
      idx_shift = idx_shift,
      idx_swing = idx_swing
    }
  end,
  draw = function()
    uf.draw = {
      get_chain_code = get_chain_code,
      draw_poly = draw_poly,
      draw_figure = draw_figure
    }
  end,
  opencv = function()
    --[[
    IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING.

     By downloading, copying, installing or using the software you agree to this license.
     If you do not agree to this license, do not download, install,
     copy or use the software.


                              License Agreement
                   For Open Source Computer Vision Library

    Copyright (C) 2000-2008, Intel Corporation, all rights reserved.
    Copyright (C) 2008-2011, Willow Garage Inc., all rights reserved.
    Third party copyrights are property of their respective owners.

    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.

      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.

      * The name of the copyright holders may not be used to endorse or promote products
        derived from this software without specific prior written permission.

    This software is provided by the copyright holders and contributors "as is" and
    any express or implied warranties, including, but not limited to, the implied
    warranties of merchantability and fitness for a particular purpose are disclaimed.
    In no event shall the Intel Corporation or contributors be liable for any direct,
    indirect, incidental, special, exemplary, or consequential damages
    (including, but not limited to, procurement of substitute goods or services;
    loss of use, data, or profits; or business interruption) however caused
    and on any theory of liability, whether in contract, strict liability,
    or tort (including negligence or otherwise) arising in any way out of
    the use of this software, even if advised of the possibility of such damage.
    ]]
    require("uf_opencv")
  end
}

return {
  init = function(names)
    local j
    for j=1,#names do
      func[names[j]]()
    end
    
    func["core"]()
  end
}
