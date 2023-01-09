wget \
    --directory-prefix="../resources" \
    --recursive \
    --page-requisites \
    --html-extension \
    --convert-links \
    --domains webassembly.github.io \
    --no-parent \
        https://webassembly.github.io/spec/core/
