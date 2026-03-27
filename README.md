# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是面向 MyBatis / MyBatis-Plus 的查询增强工具包。
框架以 `SqlContext<T>` 作为统一查询模型，提供 `voById`、`voList`、`voPage` 和 `lambdaBooster()` 等查询入口，用于组织动态 SQL 条件、排序和分页查询。

> 历史项目：[mybatis-plus-enhancer](https://github.com/bootystar/mybatis-plus-enhancer)

## 功能特性

- 统一的 `SqlContext<T>` 查询模型，适配动态 SQL 条件拼装场景
- 提供 `voById`、`voList`、`voPage` 三个核心查询入口
- 提供 `voByIds`、`voFirst`、`voUnique` 派生查询方法
- 支持 `lambdaBooster()` 链式构造条件、排序和分页参数
- 支持 Map / Bean 到 `SqlContext<T>` 的统一转换
- 支持原生 MyBatis、MyBatis-Plus、PageHelper 三类接入方式
- Mapper XML 可直接复用 `sqlbooster.conditions`、`sqlbooster.sorts` 片段

---

## Maven 依赖

```xml
<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>1.1.0</version>
</dependency>
```

当前仓库源码如未发布，请按实际分支版本调整依赖版本。

---

## 快速开始

### 1. Mapper 继承接口

MyBatis-Plus：

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpMapper;

public interface SysUserMapper extends MpMapper<SysUser, SysUserVO> {
}
```

原生 MyBatis：

```java
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

public interface SysUserMapper extends BoosterMapper<SysUser, SysUserVO> {
}
```

### 2. XML 引入 `sqlbooster` 片段

`BoosterMapper` 通过 `selectByXml(SqlContext<T> sqlContext, Object page)` 执行实际查询。
`SqlContext<T>` 为统一查询入参，XML 可直接复用内置动态片段：

```xml
<mapper namespace="com.example.mapper.SysUserMapper">

    <select id="selectByXml" resultType="com.example.vo.SysUserVO">
        SELECT a.*
        FROM sys_user a
        <where>
            <include refid="sqlbooster.conditions"/>
        </where>
        <trim prefix="ORDER BY" prefixOverrides=",">
            <include refid="sqlbooster.sorts"/>
            , a.id DESC
        </trim>
    </select>

</mapper>
```

### 3. 调用核心方法

列表查询和分页查询统一通过 `SqlContext<T>` 传入动态条件。

```java
@RestController
@RequestMapping("/user")
public class SysUserController {

    @Resource
    private SysUserMapper sysUserMapper;

    @GetMapping("/{id}")
    public SysUserVO detail(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    @PostMapping("/list")
    public List<SysUserVO> list(@RequestBody SysUserQuery query) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
                .fromBean(query)
                .toSqlContext();
        return sysUserMapper.voList(sqlContext);
    }

    @PostMapping("/page/{pageNum}/{pageSize}")
    public BPage<SysUserVO> page(@RequestBody SysUserQuery query,
                                 @PathVariable Long pageNum,
                                 @PathVariable Long pageSize) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
                .fromBean(query)
                .toSqlContext();
        return sysUserMapper.voPage(sqlContext, pageNum, pageSize);
    }
}
```

### 4. 实体实现默认 Booster 入口

实体可实现 `BoosterEntity<T, V>` 获取默认查询入口：

```java
import io.github.luminion.sqlbooster.core.BoosterEntity;

public class SysUser implements BoosterEntity<SysUser, SysUserVO> {
}
```

```java
SysUser user = new SysUser();

SysUserVO detail = user.booster().voById(1L);

List<SysUserVO> list = user.lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

---

## 可用扩展接口

| 接口 / 类型 | 说明 | 包路径 |
|---|---|---|
| `Booster` | 核心查询契约 | `io.github.luminion.sqlbooster.core` |
| `BoosterEntity` | 实体侧默认查询入口 | `io.github.luminion.sqlbooster.core` |
| `BoosterService` | Service 侧默认查询入口 | `io.github.luminion.sqlbooster.core` |
| `BoosterMapper` | 原生 MyBatis Mapper 扩展，不含分页实现 | `io.github.luminion.sqlbooster.extension.mybatis` |
| `MpMapper` | MyBatis-Plus Mapper 扩展，内置分页实现 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpService` | MyBatis-Plus Service 扩展接口 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpServiceImpl` | MyBatis-Plus Service 扩展基类 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `PhMapper` | PageHelper Mapper 扩展，内置分页实现 | `io.github.luminion.sqlbooster.extension.pagehelper` |
| `SqlBuilder` | `SqlContext<T>` 构造器 | `io.github.luminion.sqlbooster.builder` |
| `LambdaBooster` | Lambda 链式查询器 | `io.github.luminion.sqlbooster.core` |
| `BoosterRegistry` | 默认 Booster 注册表 | `io.github.luminion.sqlbooster.metadata` |

---

## 使用示例

### 1. Mapper / Builder / Lambda

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .orderByDesc(SysUser::getId)
        .toSqlContext();

SysUserVO detail = sysUserMapper.voById(1L);
List<SysUserVO> list = sysUserMapper.voList(sqlContext);
BPage<SysUserVO> page = sysUserMapper.voPage(sqlContext, 1L, 20L);

List<SysUserVO> lambdaList = sysUserMapper.lambdaBooster()
        .eq(SysUser::getState, 1)
        .like(SysUser::getUserName, "tom")
        .list();
```

### 2. 实体直连 Booster

```java
import io.github.luminion.sqlbooster.core.BoosterEntity;

public class SysUser implements BoosterEntity<SysUser, SysUserVO> {
}

List<SysUserVO> list = new SysUser().lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

### 3. 使用 `BoosterRegistry`

`BoosterRegistry` 提供默认 Booster 的注册与获取能力。

```java
import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.metadata.BoosterRegistry;

Booster<SysUser, SysUserVO> booster = BoosterRegistry.getRequiredBooster(SysUser.class, SysUserVO.class);

List<SysUserVO> list = booster.lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

### 4. 自定义 Mapper 直接接收 `SqlContext`

`SqlBuilder.toSqlContext()` 的结果可直接作为自定义 Mapper 入参：

```java
public interface SysUserCustomMapper {

    List<SysUserLiteVO> selectLite(SqlContext<SysUser> sqlContext);
}
```

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .orderByDesc(SysUser::getId)
        .toSqlContext();

List<SysUserLiteVO> list = sysUserCustomMapper.selectLite(sqlContext);
```

对应 XML：

```xml
<select id="selectLite" resultType="com.example.vo.SysUserLiteVO">
    SELECT a.id, a.user_name
    FROM sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
    </trim>
</select>
```

---

## 核心功能说明

### 1. `SqlContext<T>`

`SqlContext<T>` 是统一查询模型，用于承载：

- 查询条件
- 排序规则
- 动态参数
- 供 Mapper XML 直接消费的上下文数据

核心查询方法均围绕 `SqlContext<T>` 工作：

- `voById(id)`：主键查询
- `voList(sqlContext)`：列表查询
- `voPage(sqlContext, pageNum, pageSize)`：分页查询
- `selectByXml(sqlContext, page)`：Mapper XML 查询入口

适用场景包括前端条件透传、服务端动态条件拼装和 XML 统一消费。

### 2. 动态后缀映射

`fromMap` / `fromBean` 支持按字段后缀映射 SQL 操作符。
无后缀时默认按等值条件处理。

| 操作说明 | 后缀 | 示例 | 值类型 |
|---|---|---|---|
| 等于 | 无 | `"name": "mike"` | String, Number, Boolean |
| 不等于 | `Ne` | `"ageNe": 18` | String, Number, Boolean |
| 小于 | `Lt` | `"ageLt": 18` | Number, Date |
| 小于等于 | `Lte` | `"ageLte": 18` | Number, Date |
| 大于 | `Gt` | `"ageGt": 18` | Number, Date |
| 大于等于 | `Gte` | `"ageGte": 18` | Number, Date |
| 模糊匹配 | `Like` | `"nameLike": "mike"` | String |
| 反模糊匹配 | `NotLike` | `"nameNotLike": "mike"` | String |
| IN | `In` | `"stateIn": [1, 2, 3]` | List / Array |
| NOT IN | `NotIn` | `"stateNotIn": [1, 2, 3]` | List / Array |
| 为空 | `IsNull` | `"nameIsNull": true` | Boolean |
| 不为空 | `IsNotNull` | `"nameIsNotNull": true` | Boolean |
| 包含任意位 | `HasAnyBits` | `"roleHasAnyBits": 4` | Number |
| 包含全部位 | `HasAllBits` | `"roleHasAllBits": 4` | Number |
| 不包含位 | `HasNoBits` | `"roleHasNoBits": 4` | Number |

入参示例：

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGte": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

### 3. `SqlBuilder`

`SqlBuilder` 用于构造标准化 `SqlContext<T>`，常用入口包括：

- `fromMap(map)`
- `fromBean(bean)`
- `eq / ne / gt / gte / lt / lte`
- `like / notLike`
- `in / notIn`
- `isNull / isNotNull`
- `orderByAsc / orderByDesc`
- `toSqlContext()`

示例：

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .gte(SysUser::getAge, 18)
        .orderByDesc(SysUser::getId)
        .toSqlContext();
```

### 4. `SqlContext` 结构

基础结构：

```json
{
  "conditions": [
    { "field": "name", "value": "mike" },
    { "field": "version", "operator": ">=", "value": 1 }
  ],
  "sorts": [
    { "field": "id" },
    { "field": "age", "asc": true }
  ]
}
```

多层条件嵌套：

```json
{
  "conditions": [
    { "field": "country", "value": "china" },
    { "field": "mobile", "operator": "is_not_null", "value": true }
  ],
  "next": {
    "and": false,
    "conditions": [
      { "field": "name", "value": "mike" },
      { "field": "name", "value": "john" }
    ],
    "next": {
      "and": false,
      "conditions": [
        { "field": "age", "operator": "<", "value": 18 },
        { "field": "age", "operator": ">", "value": 60 }
      ]
    }
  }
}
```

---

## 兼容说明

- 当前推荐入口为 `lambdaBooster()`，`lambdaBuilder()` 与 `sqlBuilder()` 仅保留为兼容方法
- `SqlBuilder` 推荐使用 `toSqlContext()`，`build()` 为兼容保留方法
- 查询方法统一使用 `voById`、`voList`、`voPage`
