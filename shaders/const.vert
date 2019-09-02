#version 430 core

layout(location = 0) in vec3 vPosition;
//layout(location = 1) in vec3 uvCoords;
//uniform float fTime;

// Output data ; will be interpolated for each fragment.
//out vec3 fragCoord;
//out float time;

void main()
{
	gl_Position = vec4(vPosition.x, vPosition.y, vPosition.z, 1.0);

// The color of each vertex will be interpolated
// to produce the color of each fragment
//   fragCoord = uvCoords;
//   time = fTime;
}
