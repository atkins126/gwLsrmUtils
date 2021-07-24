Программа Spe2Spm предназначена для создания spm-файлов из 2-х spe.

Формат:
Spe2Spm.exe <SpeFile1> <SpeFile2> <SpmDir>

Результат:
Программа записывает новый spm-файл в директорию SpmDir из SpeFile1 и SpeFile2 с именем SpeFile1

Примеры использования:
1) создать файл Cs-134_2.spm в директории spm\ из файлов beta\Cs-134_2.spe и gamma\Cs-134_2.spe (полезно для Атомтех)
Spe2Spm.exe beta\Cs-134_2.spe gamma\Cs-134_2.spe spm\
2) Массовое создание spm-файлов из всех spe-файлов директории (наверное, можно придумать покрасивее)
cd beta\
for %%i in (*.spe) do ..\Spe2Spm.exe %%i ..\gamma\%%i ..\spm\
cd ..

History
0.1.0 beta	первая версия программы по созданию spm-файлов из 2-х spe. SpectrSize -- фиксирован 1024