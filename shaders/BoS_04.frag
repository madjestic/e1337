#version 450

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec3 uv;
out vec4 fragColor;

void main()
{
  vec3 iResolution  = vec3(u_resolution.x, u_resolution.y, 1.0);
  vec2 u_resolution = iResolution.xy;
  vec2 st     = vec2(uv.x, uv.y);
  vec2 mpos = vec2 (u_mouse.x/u_resolution.x, u_mouse.y/u_resolution.y);
  mpos = vec2(mpos.x, 1.0-mpos.y);

  float pct = distance(st, mpos);
  pct = smoothstep(0.1+sin(u_time/100)*0.1, 0.11+sin(u_time/100)*0.1, pct);
  
  vec3 color  = vec3(circle(st, mpos, 0.9));
  fragColor    = vec4(color, 1.0);
}
