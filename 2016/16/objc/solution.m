#import <stdio.h>
#import <Foundation/Foundation.h>


@interface TSDragonCurveDataGenerator : NSObject

- (void)loadData:(NSString *)data;
- (void)generateToFillDiskSized:(NSUInteger)size;
- (NSString *)generatedData;
- (NSString *)checksum;

@end


@implementation TSDragonCurveDataGenerator
{
    char *dataBuffer;
    NSUInteger dataIndex;
    char *checksumBuffer;
    NSUInteger checksumIndex;
}

- (instancetype)init
{
    if (self = [super init]) {
        dataBuffer = nil;
        dataIndex = 0;
        checksumBuffer = nil;
        checksumIndex = 0;
    }
    return self;
}

- (void)dealloc
{
    if (dataBuffer) {
        free(dataBuffer);
    }
    if (checksumBuffer) {
        free(checksumBuffer);
    }
}

- (void)loadData:(NSString *)data
{
    NSUInteger dataSize = [data length] + 1;
    dataIndex = dataSize - 1;
    [self _ensureDataBufferCapacityFor:dataSize];
    [data getCString:dataBuffer
           maxLength:dataSize
            encoding:NSASCIIStringEncoding];
}

- (void)generateToFillDiskSized:(NSUInteger)size
{
    [self _ensureDataBufferCapacityFor:size];
    [self _generateDataToSize:size];
}

- (void)_ensureDataBufferCapacityFor:(NSUInteger)size
{
    [self _ensureBuffer:&dataBuffer hasCapacity:size * 2];
}

- (void)_generateDataToSize:(NSUInteger)size
{
    while (dataIndex < size) {
        dataBuffer[dataIndex++] = '0';
        for (int i = dataIndex - 2; i >= 0; i--) {
            dataBuffer[dataIndex++] = dataBuffer[i] == '0' ? '1' : '0';
        }
    }
    dataIndex = size;
}

- (NSString *)generatedData
{
    dataBuffer[dataIndex] = '\0';
    NSString *result = [NSString stringWithUTF8String:dataBuffer];
    return result;
}

- (NSString *)checksum
{
    checksumIndex = 0;
    [self _ensureChecksumBufferCapacity];

    for (int i = 0; i < dataIndex; i += 2) {
        char a = dataBuffer[i];
        char b = dataBuffer[i+1];
        checksumBuffer[checksumIndex++] = (a == b) ? '1' : '0';
    }

    while (checksumIndex % 2 == 0) {
        NSUInteger i = 0;
        for (int j = 0; j < checksumIndex; j += 2) {
            char a = checksumBuffer[j];
            char b = checksumBuffer[j+1];
            checksumBuffer[i++] = (a == b) ? '1' : '0';
        }
        checksumIndex = i;
    }

    checksumBuffer[checksumIndex] = '\0';
    NSString *result = [NSString stringWithUTF8String:checksumBuffer];
    return result;
}

- (void)_ensureChecksumBufferCapacity
{
    [self _ensureBuffer:&checksumBuffer hasCapacity:dataIndex];
}

- (void)_ensureBuffer:(char **)buffer hasCapacity:(NSUInteger)capacity
{
    size_t size = capacity * sizeof(char);
    if (*buffer) {
        *buffer = realloc(*buffer, size);
    } else {
        *buffer = malloc(size);
    }
}

@end


int main(int argc, char *argv[])
{
    NSString *input = @"01000100010010111";
    TSDragonCurveDataGenerator *g = [[TSDragonCurveDataGenerator alloc] init];

    // Part 1
    [g loadData:input];
    [g generateToFillDiskSized:272];
    printf("Part 1: %s\n", [[g checksum] UTF8String]);

    // Part 2
    [g loadData:input];
    [g generateToFillDiskSized:35651584];
    printf("Part 2: %s\n", [[g checksum] UTF8String]);

    return 0;
}
