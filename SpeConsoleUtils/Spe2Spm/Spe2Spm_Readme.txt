��������� Spe2Spm ������������� ��� �������� spm-������ �� 2-� spe.

������:
Spe2Spm.exe <SpeFile1> <SpeFile2> <SpmDir>

���������:
��������� ���������� ����� spm-���� � ���������� SpmDir �� SpeFile1 � SpeFile2 � ������ SpeFile1

������� �������������:
1) ������� ���� Cs-134_2.spm � ���������� spm\ �� ������ beta\Cs-134_2.spe � gamma\Cs-134_2.spe (������� ��� �������)
Spe2Spm.exe beta\Cs-134_2.spe gamma\Cs-134_2.spe spm\
2) �������� �������� spm-������ �� ���� spe-������ ���������� (��������, ����� ��������� ����������)
cd beta\
for %%i in (*.spe) do ..\Spe2Spm.exe %%i ..\gamma\%%i ..\spm\
cd ..

History
0.1.0 beta	������ ������ ��������� �� �������� spm-������ �� 2-� spe. SpectrSize -- ���������� 1024