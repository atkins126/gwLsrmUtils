Утилита EveryDayBackgroundControl предназначена для ежедневного контроля фона по интегралке и запускается из сценариев SL
Параметры командной строки: <ResultName_from_old_Background.txt> <ResultName_from_new_Background.txt> [<ErrorLevel>]
где <ResultName_from_old_Background.txt> - имя (путь) к текстовому файлу Result, где хранятся результаты поиска пиков, выполненного SL (стандартный файл для расчёта эффективности), для старого фона
    <ResultName_from_new_Background.txt> - имя (путь) к текстовому файлу Result, где хранятся результаты поиска пиков, выполненного SL (стандартный файл для расчёта эффективности), для нового фона
    <ErrorLevel> - необязательный параметр, указывающий уровень систематической погрешности в %, которая учитывается при сравнении фонов, по умолчанию она равна 0% 
Example: EveryDayBackgroundControl.exe Result1.txt Result2.txt 0.1

Программа возращает интегральные скорости счёта для старого и нового спектров фона, разность между ними в % и Хи в стандартый output (stdout)
Example:
Old background countrate: 3.605+/-0.013 (0.37%)
New background countrate: 6.706+/-0.012 (0.19%)
Background difference: 60.142 %
Background Chi: 169.07
Your backround is much lager than old, you need find a contamination or write new backround to configuration

Из сценария в SL вызывается командой execute. Параметры командной строки можно указывать в команде или вставить в bat-файл и вызывать его

Пример сценария:
; очищаем разметку
clear
; поиск пиков
searchandfit
; сохранение в файл результатов поисков
output(C:\Lsrm\BackgroundControl\Result2.txt)
; открытие стандартного фонового спектра, всегда сохранён в Background\Background.spe
open(c:\Lsrm\Work\UVT_Gem15P4\Spe\Background\Background.spe)
; поиск пиков в текущем спектров
searchandfit
; сохранение рез-в поиска в файл
output(C:\Lsrm\BackgroundControl\Result1.txt)
; запуск программы контроля фона
execute('c:\Lsrm\BackgroundControl\EveryDayBackgroundControl.bat','','',0)
; закрытие текущего окна
close