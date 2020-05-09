#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;
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
	vec3 normal =
		vec3( N.x*0.5+0.5
			, N.y*0.5+0.5
			,-N.z*0.5+0.5 );

	//fragColor = vec4( 0.0, 0.0, 1.0, A );    // Custom
	//fragColor = vec4( Cd.x, Cd.y, Cd.z, A ); // Cd
	//fragColor = vec4( normal, 1.0);          // N
	//fragColor = vec4( uv.x, uv.y, 0.0, A );    // UV
	fragColor = vec4(texture(tex_00, vec2(uv.x, uv.y)).rgb, 1.0);
	// fColor = vec4( mix( texture(tex_00, uv).rgb,
	// 					texture(tex_01, uv).rgb, 0.5 ) * clr
	// 			   , 1.0 );
}
