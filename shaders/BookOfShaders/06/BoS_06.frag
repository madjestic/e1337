#version 450

#define PI 3.14159265359
#define TWO_PI 6.28318530718

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
	vec2 st    = gl_FragCoord.xy/u_resolution.xy;
	//vec2 st    = uv.xy;
	st.x *= u_resolution.x/u_resolution.y;
	vec3 color = vec3(0.0);
	float d = 0.0;

	// Remap the space to -1. to 1.
	st = st *2. -1.;

	// Number of sides of your shape
	int N = 15;

	// Angle and radius from the current pixel
	float a = atan(st.x, st.y) + PI;
	float r = TWO_PI/float(N);

	// Shaping function that modulate the distance
	d = max(cos(floor(.5+a/r+1)*r-a+.1),0.8)*length(st+0.1*sin(u_time/1000));

	color = vec3(1.0-smoothstep(.4, .41,d));
	// color = vec3(d);

	gl_FragColor = vec4(color, 1.0);
	//gl_FragColor = vec4(st, 0.0, 0.0);
}
