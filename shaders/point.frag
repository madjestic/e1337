#version 450

in  vec4 vPos;
in  vec3 fragCoord;
out vec4 FragColor;

uniform vec3 Color;
uniform vec3 lightDir;

void main()
{
    // calculate normal from texture coordinates
    // vec3 N;
    // //N.xy = vPos * 2.0 - vec2(1.0);
	// N    = fragCoord - vPos.xyz;//vec3(1.0);
    // float mag = dot(N, N);
    // if (mag > 2.0) discard;   // kill pixels outside circle
    // N.z = sqrt(1.0-mag);

    // // // calculate lighting
    // float diffuse = max(0.0, dot((vec3(0,1,0)), N));

	//vec3 val = vec3 ((gl_FragCoord).xy, gl_FragCoord.z);
    //FragColor = vec4 (normalize(val), 1.0);//vec4(fragCoord,1) * diffuse;//vec4(0,1,0,1) * diffuse;
	float near = 0.01;
	float far  = 2.0;
	float z    = gl_FragCoord.z;
	float linearDepth = (2.0 * near * far) / (far + near - z * (far - near));
	//FragColor = vec4(vec3(gl_FragCoord.z * 2.0 - 1.0), 1.0);
	
	FragColor = vec4(vec3(linearDepth), 1.0);
}
