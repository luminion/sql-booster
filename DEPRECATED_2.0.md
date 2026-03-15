# SQL Booster 废弃清单 (2.0 版本)

> 以下 API 将在 2.0 版本中移除，请及时迁移

## 目录
- [抽象构建器 (AbstractSqlBuilder)](#abstractsqlbuilder)
- [Lambda 构建器 (LambdaSqlBuilder)](#lambdasqlbuilder)
- [核心接口 (Booster)](#booster)

---

## AbstractSqlBuilder

**包**: `io.github.luminion.sqlbooster.builder`

### 废弃方法

| 方法 | 替代方案 | 废弃原因 |
|------|----------|----------|
| `appendEqByMap(Map<?, ?>)` | `appendByMap(Map)` | 若配置了后缀映射，映射的条件不一定为eq |
| `appendEqByBean(Object)` | `appendByBean(Object)` | 若配置了后缀映射，映射的条件不一定为eq |

---

## LambdaSqlBuilder

**包**: `io.github.luminion.sqlbooster.builder`

### 废弃方法

| 方法 | 替代方案 | 废弃原因 |
|------|----------|----------|
| `bitAny(SFunc<T, R>, R)` | `hasAnyBits(SFunc<T, R>, R)` | 方法命名语义不清晰 |
| `bitAll(SFunc<T, R>, R)` | `hasAllBits(SFunc<T, R>, R)` | 方法命名语义不清晰 |
| `bitNone(SFunc<T, R>, R)` | `hasNoBits(SFunc<T, R>, R)` | 方法命名语义不清晰 |

---

## Booster

**包**: `io.github.luminion.sqlbooster.core`

### 废弃方法

| 方法 | 替代方案 | 废弃原因 |
|------|----------|----------|
| `lambdaBuilder()` | `lambdaBooster()` | 方法命名不一致 |
| `sqlBuilder()` | `lambdaBooster()` | 方法命名不一致 |

---

## 迁移指南

### AbstractSqlBuilder 迁移示例

```java
// 废弃写法
builder.appendEqByMap(map);
builder.appendEqByBean(bean);

// 推荐写法
builder.appendByMap(map);
builder.appendByBean(bean);
```

### LambdaSqlBuilder 迁移示例

```java
// 废弃写法
query.bitAny(User::getStatus, 1);
query.bitAll(User::getStatus, 1);
query.bitNone(User::getStatus, 1);

// 推荐写法
query.hasAnyBits(User::getStatus, 1);
query.hasAllBits(User::getStatus, 1);
query.hasNoBits(User::getStatus, 1);
```

### Booster 迁移示例

```java
// 废弃写法
booster.lambdaBuilder().eq(User::getName, "test");
booster.sqlBuilder().eq(User::getName, "test");

// 推荐写法
booster.lambdaBooster().eq(User::getName, "test");
```

---
