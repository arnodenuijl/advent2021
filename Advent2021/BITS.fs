module Advent2021.BITS

open System
open Advent2021
open FParsec
open Helpers

type Packet =
| Literal of int64
| Sum of BitsMessage[]
| Product of BitsMessage[]
| Minimum of BitsMessage[]
| Maximum of BitsMessage[]
| GT of BitsMessage[]
| LT of BitsMessage[]
| EQ of BitsMessage[]

and BitsMessage = {
    Version : int32
    Packet : Packet   
}

module Parser =        
    let HexStringToBinaryStringParser: Parser<string,unit> =
        let CharToBinary (c:Char) =
            let intValue = Convert.ToInt64($"{c}", 16)
            Convert
                .ToString(intValue, 2)
                .PadLeft(4,'0') 
    
        many (anyChar |>> CharToBinary) // elk hex character los omzetten naar binary string
        |>> seqToString
    
    let OneOrZeroParser : Parser<char, unit> =
        choice [
            pchar '1' 
            pchar '0' 
        ]
            
    let binaryStringParser (bitsToRead: int): Parser<string,unit>  =
        parray bitsToRead OneOrZeroParser
        |>> seqToString

    let binaryNumberParser bitsToRead  =
        parray bitsToRead OneOrZeroParser
        |>> (seqToString  >> binaryStringToInt)
        
    let VersionParser =       
        pipe3 OneOrZeroParser OneOrZeroParser OneOrZeroParser (fun a b c -> $"{a}{b}{c}")
        |>> binaryStringToInt

    let GroupParser = 
        let notLastInGroupParser = pchar '1' >>. binaryStringParser 4
        let lastInGroupParser = pchar '0' >>. binaryStringParser 4       
        many notLastInGroupParser .>>. lastInGroupParser
        |>> fun (groups, lastGroup) ->
                [ yield! groups ; yield lastGroup]
                |> seqToString
                |> binaryStringToLong
   
    let rec PacketParser = choice [
        pstring "100" >>. GroupParser |>> Packet.Literal
        pstring "000" >>. SubPacketsParser |>> Packet.Sum
        pstring "001" >>. SubPacketsParser |>> Packet.Product
        pstring "010" >>. SubPacketsParser |>> Packet.Minimum
        pstring "011" >>. SubPacketsParser |>> Packet.Maximum
        pstring "101" >>. SubPacketsParser |>> Packet.GT
        pstring "110" >>. SubPacketsParser |>> Packet.LT
        pstring "111" >>. SubPacketsParser |>> Packet.EQ
    ]
          
    and SubPacketsParser =
        // create a charstream 
        let ParseSubstring (s: string) : Parser<BitsMessage array, unit>=
            fun (_ : CharStream<unit>) ->
                let cs = new CharStream<unit>(s, 0, s.Length, 0)
                let multipleBitsParsers = many BitsParser
                let result = (multipleBitsParsers cs).Result |> Array.ofList
                Reply(result)
            
        parse {
            let! subPacketType = anyChar
            if subPacketType = '0' then
                let! totalLength = binaryNumberParser 15
                let! subinput = parray totalLength anyChar |>> String
                let! x = ParseSubstring subinput
                return x
                
            else if subPacketType = '1' then
                let! subPacketsCount = binaryNumberParser 11
                let! x = parray subPacketsCount BitsParser
                return x
        }
    
    and BitsParser =
        VersionParser .>>. PacketParser
        |>> fun (version, packet) -> {Version = version ; Packet = packet}