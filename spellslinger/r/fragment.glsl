#version 140

in vec3 fragmentColour;
out vec3 color;

uniform sampler2D textureSampler;

void main() {
    color = fragmentColour;
}
