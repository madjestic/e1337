#version 450

in  vec3 uv;
out vec4 fragColor;

uniform float u_time;

void main()
{
  vec3 iResolution  = vec3(800, 600, 1.0);
  vec2 u_resolution = iResolution.xy;

  //vec2 st     = gl_FragCoord.xy/u_resolution.xy;
  vec2 st     = vec2(uv.x, uv.y);///u_resolution.xy;
  vec3 color  = vec3(0.0);

  //vec2  bl     = step(vec2(0.2), st);
  //float left   = step(0.05, st.x);
  float left   = smoothstep(0.0, 0.1, st.x);
  float right  = step(0.05, 1.0 - st.x);
  float top    = step(0.05, 1.0 - st.y);
  float bottom = step(0.05, st.y);
  
  float pct    = left * right * top * bottom;
  //vec2  tr     = step(vec2(0.2), 1.0-st);
  //pct *= tr.x * tr.y;
  color        = vec3(pct);
  
  fragColor    = vec4(color, 1.0);
  //fragColor = vec4( vec3(fragCoord.x,fragCoord.y,0.0), 1.0 );
  //fragColor = vec4(uv, 1.0);
}
