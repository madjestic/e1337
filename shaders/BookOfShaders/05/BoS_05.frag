#version 450

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

float plot(vec2 st, float pct, float border){
  return  smoothstep( pct-border, pct, st.y) -
          smoothstep( pct, pct+border, st.y);
}

void main()
{
	//vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	vec2 st    = uv.xy;
	vec3 color = vec3(0.0);

	vec2 pos = vec2(0.5) - st;

	float r = length(pos)*5.0*(sin(u_time/100)*0.1+0.5);
	float a = atan(pos.y, pos.x);

	//float f = cos(a*10.0*sin(u_time/1000)+u_time/100);
	//float f = abs(cos(a*10.0*sin(u_time/1000)*.5+1+u_time/100));
	//float f = abs(cos(a*2.5 + u_time/200))*.5+.3; // flower
	//float f = abs(cos(a*12. + u_time/100)*sin(a*3. + u_time/200))*.8+.1; //snowflake
	float f = smoothstep(-.5,1., cos(a*10. + u_time/50))*0.2+0.5; // gear

	f = 1.0 - smoothstep(f, f+0.1, r);
	//f = step(f+0.5, r-0.5);
	float pct = plot(st, f, 0.5);
	//color = vec3( 1.0 - smoothstep(f, f+0.1, r));
	color = vec3(pct);

	gl_FragColor = vec4(color, 1.0);
}
