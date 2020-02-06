#version 430

uniform float     u_time;
uniform vec2      u_resolution;
//uniform sampler2D tex_00;
uniform sampler2D tex_01;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	vec3 SunP  = vec3 (100000000., .0, 20000000);

	vec3 normal =
		vec3( N.x*0.5+0.5
			, N.y*0.5+0.5
			,-N.z*0.5+0.5 );
	
  //fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
  //fragColor = vec4( normal, 1.0);
  //fragColor = vec4(texture(tex_00, vec2(uv.x, uv.y)).rgb * dot(Ng, normalize(SunP)), 1.0);
  fragColor = vec4(texture(tex_01, vec2(uv.x, uv.y)).rgb, 1.0);
}
