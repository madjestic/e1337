#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
  fragColor = vec4( 0.0, 1.0, 0.0, 1.0 );
}
