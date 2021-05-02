object DmDb: TDmDb
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 154
  Width = 306
  object JvAppStore: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    AutoFlush = True
    AutoReload = True
    DefaultSection = 'Default'
    SubStorages = <>
    SynchronizeFlushReload = True
    Left = 16
    Top = 8
  end
  object AppEvents: TApplicationEvents
    OnMessage = AppEventsMessage
    Left = 16
    Top = 56
  end
end
