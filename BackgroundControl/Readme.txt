������� EveryDayBackgroundControl ������������� ��� ����������� �������� ���� �� ���������� � ����������� �� ��������� SL
��������� ��������� ������: <ResultName_from_old_Background.txt> <ResultName_from_new_Background.txt> [<ErrorLevel>]
��� <ResultName_from_old_Background.txt> - ��� (����) � ���������� ����� Result, ��� �������� ���������� ������ �����, ������������ SL (����������� ���� ��� ������� �������������), ��� ������� ����
    <ResultName_from_new_Background.txt> - ��� (����) � ���������� ����� Result, ��� �������� ���������� ������ �����, ������������ SL (����������� ���� ��� ������� �������������), ��� ������ ����
    <ErrorLevel> - �������������� ��������, ����������� ������� ��������������� ����������� � %, ������� ����������� ��� ��������� �����, �� ��������� ��� ����� 0% 
Example: EveryDayBackgroundControl.exe Result1.txt Result2.txt 0.1

��������� ��������� ������������ �������� ����� ��� ������� � ������ �������� ����, �������� ����� ���� � % � �� � ���������� output (stdout)
Example:
Old background countrate: 3.605+/-0.013 (0.37%)
New background countrate: 6.706+/-0.012 (0.19%)
Background difference: 60.142 %
Background Chi: 169.07
Your backround is much lager than old, you need find a contamination or write new backround to configuration

�� �������� � SL ���������� �������� execute. ��������� ��������� ������ ����� ��������� � ������� ��� �������� � bat-���� � �������� ���

������ ��������:
; ������� ��������
clear
; ����� �����
searchandfit
; ���������� � ���� ����������� �������
output(C:\Lsrm\BackgroundControl\Result2.txt)
; �������� ������������ �������� �������, ������ ������� � Background\Background.spe
open(c:\Lsrm\Work\UVT_Gem15P4\Spe\Background\Background.spe)
; ����� ����� � ������� ��������
searchandfit
; ���������� ���-� ������ � ����
output(C:\Lsrm\BackgroundControl\Result1.txt)
; ������ ��������� �������� ����
execute('c:\Lsrm\BackgroundControl\EveryDayBackgroundControl.bat','','',0)
; �������� �������� ����
close