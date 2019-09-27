#version 450

#ifdef GL_ES
precision mediump float;
#endif

#define PI     3.14159265358979323846
#define TWO_PI 6.28318530718

layout(location = 0) out vec4 diffuseColor;

uniform float u_time;
uniform vec2 u_mouse;
uniform vec2 u_resolution;

in  vec4 gl_FragCoord;
in  vec3 uv;
out vec4 fragColor;

float plot(vec2 st, float pct, float border)
{
  return  smoothstep( pct-border, pct, st.y) -
          smoothstep( pct, pct+border, st.y);
}

float circle(in vec2 _st, in float _radius)
{
	vec2 l = _st - vec2(0.5);
	return 1. - smoothstep(_radius - _radius*0.01,
						   _radius + _radius*0.01,
		                   dot(l,l)*4.0);
}

vec2 rotate2D(vec2 _st, float _angle)
{
	_st -= 0.5;
	_st = mat2(cos(_angle), -sin(_angle),
		       sin(_angle),  cos(_angle)) * _st;
	_st += 0.5;
	return _st;
}

float box(vec2 _st, vec2 _size, float _smoothEdges)
{
	_size = vec2(0.5) - _size * 0.5;
	vec2 aa = vec2(_smoothEdges * 0.5);
	vec2 uv = smoothstep(_size, _size+aa, _st);
	uv     *= smoothstep(_size, _size+aa, vec2(1.0) - _st);
	return uv.x * uv.y;
}

void main()
{
	vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	vec3 color = vec3(0.0);

	st = rotate2D(st, PI*u_time*0.00001);   // rotate
	st /= vec2(2.15,0.65)/1.5;
	st *= 5.;                              // zoom
	st = rotate2D(st, PI*u_time*0.0002);   // rotate
	st.x += step(1., mod(st.y,2.0)) * 0.5*u_time*(-0.001); // offset rows
	st = fract (st);                       // tile
	//st /= vec2(2.15,0.65)/1.5;

	color = vec3(circle(st, 0.5));
	//color = vec3(box(st, vec2(0.95, 0.85), 0.05));
	//color = vec3(st,0.0);

	fragColor = vec4(color, 1.0);
}
