unit uEntClasses;

interface
uses Classes, SysUtils, uEntities;

const
  CID_TEST = 'C798DB28-A30E-40D2-8720-DC475BD2B2A2';
//  MID_TEST_SELECT = '31911854-EE13-4A89-A4F7-CAC9355CD428';
//  MID_TEST_INSERT = '52A0C079-3283-42B5-B1F9-A181ACC7EEA6';
//  MID_TEST_DELETE = '4A6B411C-D9E1-4A7F-BB8B-84DE8CF1FB6A';
//  MID_TEST_UPDATE = 'EF2F7809-2184-4776-AEF6-C3F57887A8F3';

type
  [Select]
  [Insert]
  [Delete]
  [Update]
  [GetRow]
  [Entity(CID_TEST, 'Тест')]
  TestEntityAttribute = class(TEntity);

implementation

end.
