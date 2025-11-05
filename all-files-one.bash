# Solo para Lex.x, Parser.y, Desugar.hs, MiniLisp.hs y Interp.hs
# Guarda el contenido de los archivos en un archivo de texto llamado all-files-one.txt

for f in Lex.x Parser.y Desugar.hs MiniLisp.hs Interp.hs; do
    echo "=== File: $f ===" >> all-files-one.txt
    cat "$f" >> all-files-one.txt
    echo -e "\n" >> all-files-one.txt
done