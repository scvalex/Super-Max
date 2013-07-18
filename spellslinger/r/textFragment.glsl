#version 140

in vec2 UV;
out vec3 color;

uniform sampler2D textureSampler;

void main() {
    color = texture2D(textureSampler, UV).rgb;
    /* color = vec3(0, 0.4, 0); */
}
