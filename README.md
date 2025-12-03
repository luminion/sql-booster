# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是一个数据库查询设计的增强工具包，旨在简化和增强数据访问层的开发。提供了强大的动态SQL动态条件和后缀查询映射功能。

## 功能特性

- **后缀动态映射SQL**：支持`字段`+`后缀`自动映射不同类型查询
- **条件动态拼装SQL**：支持根据入参动态拼接条件
- **Map查询条件**：自动转化Map参数
- **数据字段映射**：自动转换属性为数据库字段
- **SQL反注入**：通过预编译SQL, 防止SQL注入
- **Lambda链式调用**：支持链式调用追加参数条件
- **VO类型转化**：自动将查询结果转化为指定类
- **额外查询方法**：添加`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`、`voPage`等方法

---

## Maven 依赖

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)

```xml

<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>latest</version>
</dependency>
```

迭代优化中,目前发布为快照版本, 可添加maven中央快照仓库(可能需网络代理)获取

老版本地址: https://github.com/bootystar/mybatis-plus-enhancer

```xml

<repositories>
    <repository>
        <name>Central Portal Snapshots</name>
        <id>central-portal-snapshots</id>
        <url>https://central.sonatype.com/repository/maven-snapshots/</url>
        <releases>
            <enabled>false</enabled>
        </releases>
        <snapshots>
            <enabled>true</enabled>
        </snapshots>
    </repository>
</repositories>

<dependencies>
    <dependency>
        <groupId>io.github.luminion</groupId>
        <artifactId>sql-booster</artifactId>
        <version>1.0.0-SNAPSHOT</version>
    </dependency>
</dependencies>

```

---

## 快速开始

[code-generator](https://github.com/luminion/code-generator)代码生成器已适配该框架, 可以一键生成代码, 开箱即用

### 1. 创建实体类

若已有实体类, 可忽略该步骤

```java
// 数据库对应的实体类
public class SysUser {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```

```java
// 用于封装查询结果的VO类, 可以继承自实体类, 也可以直接使用实体类
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```

### 2. mapper对应xml文件引用sql片段

获取xml文件内容

```java
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapperUtils;

public static void main(String[] args) {
    String mapperContent = BoosterMapperUtils.getMapperContent(SysUser.class, SysUserVO.class);
    System.out.println(mapperContent);
}
```

将获取的内容粘贴到mapper.xml文件中, 并根据需要`连表`/`添加条件`/`添加排序`

```xml
<!--复制工具类生成的该sql片段到mapper.xml文件中-->
<select id="selectByXml" resultType="com.example.test.vo.SysUserVO">
    SELECT
    a.*
    FROM
    sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
        <!--此处编写自定义条件SQL, 未自动映射的条件可通过param1.extra获取, 编写时以AND开头以兼容自动映射的查询条件-->
        AND a.deleted = 0
        <if test="param1.extra.userDeptName != null">
            AND a.dept_id in (SELECT id FROM sys_department WHERE name = #{param1.extra.userDeptName})
        </if>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
        <!--此处编写排序字段SQL, 编写时以,开头以兼容自动映射的排序-->
        , a.created_time DESC , a.id DESC
    </trim>
</select>
```

### 继承或引入指定类(接口)

继承后可获得[Booster.java](src/main/java/io/github/luminion/sqlbooster/core/Booster.java)提供的所有方法

| 接口名            | 说明                                                    | 所在包                                                 |
|----------------|-------------------------------------------------------|-----------------------------------------------------|
| BoosterMapper  | 基于mybatis的Mapper, 添加了Booster相关功能(不含分页)                | io.github.luminion.sqlbooster.extension.mybatis     |
| PhMapper       | 基于PageHelper的Mapper, 添加了Booster相关功能(基于PageHelper分页)   | io.github.luminion.sqlbooster.extension.pagehelper  |
| MpMapper       | 基于mybatis-plus的BaseMapper, 添加了Booster相关功能(基于IPage分页)  | io.github.luminion.sqlbooster.extension.mybatisplus |
| MpService      | 基于mybatis-plus的IService, 添加了Booster相关功能(基于IPage分页)    | io.github.luminion.sqlbooster.extension.mybatisplus |
| MpServiceImpl  | 基于mybatis-plus的ServiceImpl, 添加了Booster相关功能(基于IPage分页) | io.github.luminion.sqlbooster.extension.mybatisplus |
| BoosterSupport | 自定义扩展的基础, 提供了大部分默认实现, 需子类实现核心方法                       | io.github.luminion.sqlbooster.core                  |

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpMapper;

// 继承MpMapper
// eg: MpMapper已继承BaseMapper, SysUserMapper无需继承原BaseMapper
public interface SysUserMapper extends MpMapper<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpServiceImpl;

// 继承MpServiceImpl
// eg: MpServiceImpl已继承ServiceImpl, SysUserServiceImpl无需继承原ServiceImpl
public class SysUserServiceImpl extends MpServiceImpl<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpService;

// 继承MpService
// eg: MpService已继承IService, SysUserService无需继承原IService
public class SysUserService extends MpService<SysUser, SysUserVO> {

}
```

<br/>

## 使用示例

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/user")
public class SysUserController {

    // 此处引入Mapper接口或Service接口
    @Autowired
    private BoosterMapper<SysUser, SysUserVO> sysUserMapper;

    // 根据ID查询VO
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    // 通过DTO对象查询
    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        // 使用SqlBuilder可链式调用,快速组合条件,执行查询 
        SqlContext<SysUser> sqlContext = SqlBuilder.of(sysUserMapper)
                .appendEqByBean(dto) // 将DTO对象中的属性和值, 作为等于条件添加
                .build(); // build 方法返回SqlContext, 在build的时候会严格对参数进行校验移除无效的参数
        return sysUserMapper.voList(sqlContext);
    }

    // 通过map条件查询(支持后缀映射不同类型查询)
    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        // Booster提供sqlBuilder()方法支持链式调用, 可快速组合条件, 执行查询
        return sysUserMapper.sqlBuilder() 
                .appendEqByMap(params) // 添加Map条件
                .list();
    }

    // 使用SqlContext作为参数时, 前端可通过入参动态指定条件及排序
    @PostMapping("/sql")
    public List<SysUserVO> getUsersBySql(@RequestBody SqlContext<SysUser> sqlContext) {
        // SqlContext允许前端自定义条件和排序, 灵活性Max
        return sysUserMapper.voList(sqlContext);
    }

    // lambda调用,添加必要条件, 例如权限角色等
    @PostMapping("/lambda")
    public List<SysUserVO> getUsersBySql(@RequestBody Map<String, Object> params) {
        return sysUserMapper.sqlBuilder()
                .appendEqByMap(params) // 合并Map条件
                .eq(SysUser::getState, 1) // state=1
                .gte(SysUser::getAge, 18) // age>=18
                .in(SysUser::getRoleId, Arrays.asList(1, 2))
                .like(SysUser::getUserName, "tom") // userName like '%tom%'
                .list();
    }

    // 分页查询
    @PostMapping("/page/{current}/{size}")
    public BPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable("current") Long current,
                                        @PathVariable("size") Long size) {
        return sysUserMapper.sqlBuilder()
                .appendEqByMap(params)
                .page(current, size);
    }

}
```

---

## 核心功能

### 动态后缀映射SQL

- 在`参数`名称后添加特殊的后缀, 可以`动态映射`为`不同类型`的查询
- 在不添加后缀时, 等同于`等于`查询
- 后端可用`实体类`或`Map`接收参数

#### 动态后缀映射表

| sql操作符号       | W操作说明      | 后缀                           | 示例 (JSON Key)                                           | 值类型                         |
|---------------|------------|------------------------------|---------------------------------------------------------|-----------------------------|
| `=`           | 等于         | (无)                          | `"name": "mike"`                                        | String, Number, Boolean     |
| `<>`          | 不等于        | `Ne` / `_ne`                 | `"ageNe": 18` / `"age_ne": 18`                          | String, Number, Boolean     |
| `<`           | 小于         | `Lt` / `_lt`                 | `"ageLt": 18` / `"age_lt": 18`                          | Number, Date                |
| `<=`          | 小于等于       | `Lte` / `_lte`               | `"ageLte": 18` / `"age_lte": 18`                        | Number, Date                |
| `>`           | 大于         | `Gt` / `_gt`                 | `"ageGt": 18` / `"age_gt": 18`                          | Number, Date                |
| `>=`          | 大于等于       | `Gte` / `_gte`               | `"ageGte": 18` / `"age_gte": 18`                        | Number, Date                |
| `LIKE`        | 模糊匹配       | `Like` / `_like`             | `"nameLike": "mike"` / `"name_like": "mike"`            | String                      |
| `NOT LIKE`    | 反模糊匹配      | `NotLike` / `_not_like`      | `"nameNotLike": "mike"` / `"name_not_like": "mike"`     | String                      |
| `IN`          | IN 查询      | `In` / `_in`                 | `"stateIn": [1, 2, 3]` / `"state_in": [1, 2, 3]`        | List/Array (String, Number) |
| `NOT IN`      | NOT IN 查询  | `NotIn` / `_not_in`          | `"stateNotIn": [1, 2, 3]` / `"state_not_in": [1, 2, 3]` | List/Array (String, Number) |
| `IS NULL`     | 为空         | `IsNull` / `_is_null`        | `"nameIsNull": true` / `"name_is_null": true`           | Boolean (true)              |
| `IS NOT NULL` | 不为空        | `IsNotNull` / `_is_not_null` | `"nameIsNotNull": true` / `"name_is_not_null": true`    | Boolean (true)              |
| `& ? > 0`     | 包含任意指定bit位 | `BitAny` / `_bit_any`        | `"permissionBitAny": 4` / `"permission_bit_any": 4`     | Number                      |
| `& ? = ?`     | 包含所有指定bit位 | `BitAll` / `_bit_all`        | `"permissionBitAll": 4` / `"permission_bit_all": 4`     | Number                      |
| `& ? = 0`     | 不包含指定bit位  | `BitNone` / `_bit_none`      | `"permissionBitNone": 4` / `"permission_bit_none": 4`   | Number                      |

#### 入参示例

查询`name`包含`mike`, `version`为`1`, `age`在`18-60`之间, `state`为`1`或`2`或`3`数据:

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGte": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

#### 自定义后缀映射

- 修改`SqlContextUtils`的默认后缀, 来改变默认的后缀映射
- `SqlBuidler`类,提供带参数的`build`方法构建 `SqlContext`
- 修改`Booster`或`BoosterSupport`默认的校验
- 可用操作符见[动态后缀映射表](#动态后缀映射表)

自定义示例:

```java

import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.SqlContextUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;

public class Test {
    
    // 注入Booster的实现类, 例如SysUserMapper或SysUserService
    @Autowired
    private Booster<SysUser, SysUserVO> booster;

    public void test() {
        // 指定后缀和操作符的映射关系
        HashMap<String, String> map = new HashMap<>();
        map.put("_like", "LIKE");
        map.put("_ge", ">=");
        map.put("_le", "<=");
        map.put("_not_eq", "<>");

        // 1.设置默认后缀映射
        SqlContextUtils.refreshDefaultSuffixes(map);

        // 2.构建时指定后缀映射, 使用构建后的SqlContext作为查询条件
        SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
                .build(entityClass, sqlContext -> 
                        SqlContextUtils.buildWithSuffix(entityClass, sqlContext, map)
        );
        booster.voList(sqlContext);
        
        // 3. 重写Booster的核心逻辑, 例如boosterMapper
        new BoosterMapper<>(){
            @Override
            public List<V> selectByBooster(SqlContext<T> boosterParam, Object page) {
                // 此处为BoosterMapper默认的校验,使用SqlContextUtils提供的默认后缀, 可自行重写修改
                // 例如直接使用无参build就不再映射后缀
                // 或全局修改SqlContextUtils.refreshDefaultSuffixes(map);
                SqlContext<T> sqlContext = SqlBuilder.of(this)
                        .append(boosterParam)
                        .build(SqlContextUtils::buildWithSuffix);
                return selectByXml(sqlContext, page);
            }
        };
        
    }
}
```

### 动态条件组合SQL

- 入参自由指定`查询条件`/`查询类型`/`查询值`
- 入参自由指定`排序条件`和`升降序`
- 支持嵌套`子条件`和`or`条件
- 自动验证参数, 防止`SQL注入`
- 支持复杂条件自由组合

#### 入参格式

支持动态sql的入参类为[SqlContext](src/main/java/io/github/luminion/sqlbooster/model/SqlContext.java)
其基础格式如下:

```json
{
  "conditions": [
    {
      "field": "name",
      "operator": "=",
      "value": "mike"
    }
  ],
  "sorts": [
    {
      "field": "id",
      "asc": true
    }
  ]
}
```

属性说明:

* `conditions`用于存放一组查询条件,
    * `field` 指定需要查询的字段
    * `operator` 指定查询类型, 不传递时默认为`=`
    * `value` 指定查询的值
* `sorts`用于存放一组排序条件
    * `field`指定需要排序的字段
    * `asc`指定是否升序, `true`为升序,`false`为降序, 不传递时默认为`false`

#### 查询类型映射表

| sql操作符号       | 说明         | operator参数值                  | 值类型                         |
|:--------------|:-----------|:-----------------------------|:----------------------------|
| `=`           | 等于         | 无需传参                         | Any                         |
| `<>`          | 不等于        | `<>`, `!=`, `ne`             | Any                         |
| `>`           | 大于         | `>`, `gt`                    | Number, Date                |
| `>=`          | 大于等于       | `>=`, `gte`                  | Number, Date                |
| `<`           | 小于         | `<`, `lt`                    | Number, Date                |
| `<=`          | 小于等于       | `<=`, `lte`                  | Number, Date                |
| `LIKE`        | 模糊匹配       | `like`                       | String                      |
| `NOT LIKE`    | 反模糊匹配      | `not_like`, `not like`       | String                      |
| `IN`          | 在指定列表中     | `in`                         | List/Array (String, Number) |
| `NOT IN`      | 不在指定列表中    | `not_in`, `not in`           | List/Array (String, Number) |
| `IS NULL`     | 为空         | `is_null`, `is null`         | Boolean (true)              |
| `IS NOT NULL` | 不为空        | `is_not_null`, `is not null` | Boolean (true)              |
| `& ? > 0`     | 包含任意指定bit位 | `bit_any`, `bit any`         | Number                      |
| `& ? = ?`     | 包含所有指定bit位 | `bit_all`, `bit all`         | Number                      |
| `& ? = 0`     | 不包含指定bit位  | `bit_none`, `bit none`       | Number                      |

> **提示: 其中 `=` 为默认值, 当查询的类型为`=`时无需传递operator参数**

#### 基本使用

查询`name`为`mike`, `version`大于等于`1`, `state`为`1`或`2`或`3`的数据

```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "operator": ">=",
      "value": 1
    },
    {
      "field": "state",
      "operator": "IN",
      "value": [1, 2, 3]
    }
  ]
}
```

#### 指定排序字段

查询`name`为`mike`, `version`为`1`的数据, 并将结果按照`id`降序, `age`升序排列

```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "value": 1
    }
  ],
  "sorts": [
    {
      "field": "id"
    },
    {
      "field": "age",
      "asc": true
    }
  ]
}
```

#### 完整功能 / 复杂查询

上文内容简化了`SqlContext`结构, 实际结构为:

```json
{
  "conditions": [
    {
      "field": "name",
      "operator": "=",
      "value": "mike"
    },
    {
      "field": "age",
      "operator": ">=",
      "value": 18
    }
  ],
  "and": true,
  "next": {
    "conditions": [],
    "and": false,
    "next": null
  },
  "sorts": [
    {
      "field": "id"
    },
    {
      "field": "age",
      "asc": true
    }
  ]
}
```

字段属性说明:


* `conditions`用于存放一组查询条件,
    * `field` 指定需要查询的字段
    * `operator` 指定查询类型, 不传递时默认为`=`
    * `value` 指定查询的值
* `and`表示当前层级中`conditions`中多个条件间的关系
    * `true`默认值(), 表示并且, 必须同时满足当前层级`conditions`所有条件
    * `false`表示或, 满足当前层级`conditions`中任意一个条件即可
    * 该值无需传递, 不传递时默认`true`
* `next`为可嵌套的子条件, `仅复杂条件时`
    * 无需嵌套时可忽略该属性
    * 嵌套层级下不接受`sorts`参数
    * 必要时,嵌套层级下还可继续嵌套`next`
* `sorts`用于存放一组排序条件
    * `field`指定需要排序的字段
    * `asc`指定是否升序, `true`为升序,`false`为降序, 不传递时默认为`false`

最佳实践:
- 需要同时满足的条件都放在根节点的`conditions`中
- `and`默认为`true`,90%情况下无需传递
- `next`仅用于嵌套或关系的条件, 90%情况下无需传递
- 当同时遇到既需要满足几个必要条件, 又存在一组多个条件任意满足其一时, 将这组条件放入一个`next`节点中,指定`and`为false

```json
{
  "conditions": [],
  "sorts": [],
  "child": {
    "conditions": [],
    "and": false,
    "next": {
      "conditions": [],
      "and": false
    }
  }
}
```

复杂条件示例: 查询国籍为中国, 手机号不为空,姓名为mike或者john, 年龄小于18岁或者大于60岁的数据

```sql
SELECT *
FROM sys_user
WHERE (country = 'china' AND mobile IS NOT NULL)
  AND (name = 'mike' OR name = 'john')
  AND (age < 18 OR age > 60)
```

输入参数:

```json
{
  "conditions": [
    {
      "field": "country",
      "value": "china"
    },
    {
      "field": "mobile",
      "operator": "is_not_null",
      "value": true
    }
  ],
  "next": {
    "and": false,
    "conditions": [
      {
        "field": "name",
        "value": "mike"
      },
      {
        "field": "name",
        "value": "john"
      }
    ],
    "next": {
      "and": false,
      "conditions": [
        {
          "field": "age",
          "operator": "<",
          "value": 18
        },
        {
          "field": "age",
          "operator": ">",
          "value": 60
        }
      ]
    }
  }
}
```