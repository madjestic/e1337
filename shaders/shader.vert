#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 uvCoords;
uniform float u_time;
uniform mat4  persp;
uniform mat4  camera;
uniform mat4  transform;

// Output data ; will be interpolated for each fragment.
out vec3 fragCoord;
out float time;

void main()
{
	gl_Position = persp * (camera * transform) * vPosition;
	fragCoord   = uvCoords;
	time        = u_time;
}
