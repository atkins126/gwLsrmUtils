# gwLsrmUtils

**gwLsrmUtils** -- утилиты для выполнения вспомогательных задач для спектрометрии с ПО LSRM (Delphi7 source code).

- **EffCenter** -- программа для расчёта расстояния до эффективного центра детектора.
- **spe_console_utils** -- утилиты для работы с файлами spe:
  - spe2txtconverter -- конвертирует spe <-> txt, 
  - speHeadEdit -- редактирует параметры шапки spe
  - Spe2Spm -- объединяет спектры в spm
- **ResultReader**  -- сохранение пиков из result.txt в колонки, разделённые табом,
- **ZoneReader** -- чтение информации о зонах
- **EveryDayBackgroundControl** -- контроль фона по интегралке
- **NuclideCalc** -- программа для пересчёта активности на дату; 
- **LibEditor** -- утилита для чтения библиотек, сортировки линий и нуклидов, конвертирования, сшивки; 
- **MuDat** - программа для просмотра коэффициентов поглощения для гамма, 



**Dependencies**
Утилиты написаны на Delphi (компилируются Delphi 7). 
Зависят от библиотек:

- jedi lib (https://www.delphi-jedi.org/index.html)
- toolbar 2000 (https://jrsoftware.org/tb2k.php)
